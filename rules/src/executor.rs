pub mod bytecode;
pub mod code_generation;
pub mod error;
pub mod rust_funcs;
pub mod semantic_analysis;
pub mod value;

use crate::{
    executor::value::{Value, ValueFrom, ValueType},
    model::Game,
};
use crate::{
    executor::{
        bytecode::Bytecode,
        error::RuntimeError,
        semantic_analysis::hm::{
            type_schemes::TypeScheme,
            types::{Type, TypeVariable},
        },
    },
    parsing::{parse_type_scheme, Span},
};
use lazy_static::lazy_static;
use maplit::hashmap;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Display};

#[derive()]
pub struct Executor {
    pub stack: ExecutorStack,
    pub heap: ExecutorHeap,
    pub bytecode: Vec<Bytecode>,
    pub ip: usize,
    pub ip_stack: Vec<usize>,
    pub labels: HashMap<String, usize>,
}

#[derive(Default, Debug)]
pub struct ExecutorStack {
    stack: Vec<Value>,
}

#[derive(Default, Debug)]
pub struct ExecutorHeap {
    heap: HashMap<usize, Value>,
}

impl Executor {
    pub fn advance(&mut self, game: &mut Game) -> Result<String, RuntimeError> {
        let code = self
            .bytecode
            .get(self.ip)
            .ok_or(RuntimeError::InvalidBytecodeOffset)?;

        let mut ret = String::new();

        let advance_intruction_pointer = match code {
            Bytecode::Print => {
                let value = self.stack.pop_any()?;
                match value {
                    Value::ArrayLength(len) => {
                        self.stack.push(len);

                        let arr = self.stack.pop_any_array()?;
                        ret = format!(
                            "{}",
                            arr.iter()
                                .map(ToString::to_string)
                                .collect::<Vec<_>>()
                                .join(" ")
                        );
                        self.stack.push_any_array(arr);
                    }
                    value => {
                        ret = format!("{}", &value);
                        self.stack.push_any(value);
                    }
                }
                true
            }
            Bytecode::Call(name) => {
                if let Some(func) = RUST_FN.get(name.as_str()) {
                    func(self, game)?;
                    true
                } else if let Some(label) = self.labels.get(name) {
                    self.ip_stack.push(self.ip);
                    self.ip = *label;
                    false
                } else {
                    return Err(RuntimeError::InvalidFn(name.clone()));
                }
            }
            Bytecode::Load(value) => {
                self.stack.push_any(value.clone());

                true
            }
            Bytecode::Jump(new_ip) => {
                self.ip = *new_ip;
                false
            }
            Bytecode::JumpIf(new_ip) => {
                if self.stack.pop()? {
                    self.ip = *new_ip;
                    false
                } else {
                    true
                }
            }
            Bytecode::Return => {
                self.ip = self
                    .ip_stack
                    .pop()
                    .ok_or(RuntimeError::NoInstructionPointer)?;
                true
            }
            // NO-OP
            Bytecode::Label(_) => true,
            Bytecode::Store(idx) => {
                let value = self.stack.pop_any()?;
                self.heap.store(*idx, value);

                true
            }
            Bytecode::LoadRef(idx) => {
                self.stack.push_any(self.heap.load(idx)?);

                true
            }
            Bytecode::Unload(idx) => {
                self.heap.unload(idx)?;

                true
            }
        };

        if advance_intruction_pointer {
            self.ip += 1;
        }

        Err(RuntimeError::Unfinished(ret))
    }
}

impl ExecutorHeap {
    pub fn store(&mut self, key: usize, value: Value) {
        self.heap.insert(key, value);
    }

    pub fn load(&mut self, key: &usize) -> Result<Value, RuntimeError> {
        if let Some(value) = self.heap.get(key) {
            Ok(value.clone())
        } else {
            Err(RuntimeError::MissingHeapValue(*key))
        }
    }
    pub fn unload(&mut self, key: &usize) -> Result<(), RuntimeError> {
        if self.heap.remove(key).is_none() {
            Err(RuntimeError::MissingHeapValue(*key))
        } else {
            Ok(())
        }
    }
}

impl ExecutorStack {
    pub fn push<V: Into<Value>>(&mut self, value: V) {
        self.stack.push(value.into());
    }

    pub fn pop<V: ValueFrom>(&mut self) -> Result<V, RuntimeError> {
        self.stack
            .pop()
            .ok_or(RuntimeError::EmptyStack)
            .and_then(V::try_from)
    }
    pub fn pop_opt<V: ValueFrom>(&mut self) -> Result<Option<V>, RuntimeError> {
        let value = self.stack.pop().ok_or(RuntimeError::EmptyStack)?;
        match value {
            Value::None => Ok(None),
            value => V::try_from(value).map(Some),
        }
    }

    pub fn push_array<V: Into<Value>>(&mut self, values: Vec<V>) {
        let length = values.len();
        for value in values {
            self.push(value);
        }

        self.push(length);
    }
    pub fn pop_array<V: ValueFrom>(&mut self) -> Result<Vec<V>, RuntimeError> {
        let length = self.pop::<usize>()?;
        self.stack
            .drain(..self.stack.len() - length)
            .map(V::try_from)
            .collect()
    }
    pub fn push_any_array(&mut self, values: Vec<Value>) {
        let length = values.len();
        for value in values.into_iter() {
            self.push(value);
        }

        self.push(length);
    }
    pub fn pop_any_array(&mut self) -> Result<Vec<Value>, RuntimeError> {
        let length = self.pop::<usize>()?;
        Ok(self.stack.drain(self.stack.len() - length..).collect())
    }

    pub fn push_any(&mut self, value: Value) {
        self.stack.push(value)
    }
    pub fn pop_any(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::EmptyStack)
    }
}

pub type ExecutorFn = fn(&mut Executor, &mut Game) -> Result<(), RuntimeError>;
lazy_static! {
    pub static ref RUST_FN: HashMap<&'static str, ExecutorFn> = hashmap! {
        "card" => rust_funcs::card as ExecutorFn,
        "move" => rust_funcs::move_card as ExecutorFn,
        "some" => rust_funcs::some as ExecutorFn,
    };
    pub static ref RUST_FN_TYPE_SCHEMES: HashMap<&'static str, TypeScheme<'static>> = {
        hashmap! {
            "card" => parse_type_scheme(Span::new("fn(i32) -> card")).unwrap().1,
            "move" => parse_type_scheme(Span::new("fn(card, zone) -> zone")).unwrap().1,
            "print" => parse_type_scheme(Span::new("fn(T) -> T")).unwrap().1,
            "some" => parse_type_scheme(Span::new("fn(T) -> ?T")).unwrap().1,
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct FnTypeInfo {
    pub argument_types: Vec<ValueType>,
    pub return_type: ValueType,
}
impl Display for FnTypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> {}",
            self.argument_types
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", "),
            self.return_type
        )
    }
}

// use crate::model::Color;
// use std::{borrow::Borrow, collections::HashMap};
//
// #[derive(Default)]
// pub struct Scope<'a> {
//     parent: Option<&'a mut Scope<'a>>,
//     colors: HashMap<String, Color>,
//     integers: HashMap<String, i32>,
// }

// pub trait ScopedType {
//     fn get<'this>(scope: &'this Scope<'_>, key: &str) -> Option<&'this Self>;
//     fn set<'this>(self, scope: &'this mut Scope<'_>, key: String);
// }

// impl<'a> Scope<'a> {
//     pub fn get<'this, T: ScopedType>(&'this self, key: &str) -> Option<&'this T> {
//         T::get(self, key).or_else(|| self.parent.as_ref().and_then(|scope| scope.get(key)))
//     }
//     pub fn set<'this, T: ScopedType>(&'this mut self, key: String, value: T) {
//         value.set(self, key)
//     }
//     pub fn child_scope<'this: 'a>(&'this mut self) -> Scope<'this> {
//         Scope {
//             parent: Some(self),
//             ..Self::default()
//         }
//     }
// }

// macro_rules! impl_scoped_type {
//     ($impl_type:ty, $field:ident) => {
//         impl ScopedType for $impl_type {
//             fn get<'this>(scope: &'this Scope<'_>, key: &str) -> Option<&'this Self> {
//                 scope.$field.get(key.borrow())
//             }

//             fn set<'this>(self, scope: &'this mut Scope<'_>, key: String) {
//                 scope.$field.insert(key, self);
//             }
//         }
//     };
// }

// impl_scoped_type!(i32, integers);
// impl_scoped_type!(Color, colors);
