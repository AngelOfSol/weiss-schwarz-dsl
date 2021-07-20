pub mod bytecode;
pub mod code_generation;
pub mod error;
pub mod rust_funcs;
pub mod semantic_analysis;
pub mod value;

use crate::executor::{bytecode::ExecutableBytecode, error::RuntimeError, value::Label};
use crate::{
    executor::value::{Value, ValueFrom, ValueType},
    model::Game,
};
use lazy_static::lazy_static;
use maplit::hashmap;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Display};

#[derive()]
pub struct Executor {
    pub stack: ExecutorStack,
    pub heap: ExecutorHeap,
    pub bytecode: Vec<ExecutableBytecode>,
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
    heap: HashMap<String, Vec<Value>>,
}

impl Executor {
    pub fn advance(&mut self, game: &mut Game) -> Result<String, RuntimeError> {
        let code = self
            .bytecode
            .get(self.ip)
            .ok_or(RuntimeError::InvalidBytecodeOffset)?;

        let mut ret = String::new();

        let advance_intruction_pointer = match code {
            ExecutableBytecode::Print => {
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
            ExecutableBytecode::Call(name) => {
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
            ExecutableBytecode::Load(value) => {
                self.stack.push_any(value.clone());
                true
            }
            ExecutableBytecode::Jump(new_ip) => {
                self.ip = *new_ip;
                false
            }
            ExecutableBytecode::JumpIf(new_ip) => {
                if self.stack.pop()? {
                    self.ip = *new_ip;
                    false
                } else {
                    true
                }
            }
            ExecutableBytecode::Return => {
                self.ip = self
                    .ip_stack
                    .pop()
                    .ok_or(RuntimeError::NoInstructionPointer)?;
                true
            }
            // NO-OP
            ExecutableBytecode::Label(_) => true,
            ExecutableBytecode::Store(idx) => {
                let value = self.stack.pop_any()?;
                self.heap.store(idx.clone(), value);

                true
            }
            ExecutableBytecode::LoadRef(idx) => {
                self.stack.push_any(self.heap.load(idx)?);

                true
            }
            ExecutableBytecode::Unload(idx) => {
                self.heap.unload(idx)?;

                true
            }

            ExecutableBytecode::CallDynamic => {
                let value = self.stack.pop_any()?;

                match value {
                    Value::Label(Label { ip, .. }) => {
                        self.ip_stack.push(self.ip);
                        self.ip = ip;
                        false
                    }
                    Value::RustFn(name) => {
                        if let Some(func) = RUST_FN.get(name) {
                            func(self, game)?;
                        } else {
                            return Err(RuntimeError::InvalidFn(name.to_string()));
                        }
                        true
                    }
                    value => return Err(RuntimeError::InvalidCallDynamic(value)),
                }
            }
        };

        if advance_intruction_pointer {
            self.ip += 1;
        }

        Err(RuntimeError::Unfinished(ret))
    }
}

impl ExecutorHeap {
    pub fn store(&mut self, key: String, value: Value) {
        let data = self.heap.entry(key).or_default();
        data.push(value);
    }

    pub fn load(&mut self, key: &str) -> Result<Value, RuntimeError> {
        if let Some(value) = self.heap.get(key).and_then(|internal| internal.last()) {
            Ok(value.clone())
        } else {
            Err(RuntimeError::MissingHeapValue(key.to_string()))
        }
    }
    pub fn unload(&mut self, key: &str) -> Result<(), RuntimeError> {
        if let Some(_) = self.heap.get_mut(key).and_then(|inner| inner.pop()) {
            Ok(())
        } else {
            Err(RuntimeError::MissingHeapValue(key.to_string()))
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
    pub(crate) static ref RUST_FN: HashMap<&'static str, ExecutorFn> = hashmap! {
        "card" => rust_funcs::card as ExecutorFn,
        "move" => rust_funcs::move_card as ExecutorFn,
        "some" => rust_funcs::some as ExecutorFn,
        "or_default" => rust_funcs::or_default as ExecutorFn,
        "+" => rust_funcs::add as ExecutorFn,
        "-" => rust_funcs::sub as ExecutorFn,
        "==" => rust_funcs::eq as ExecutorFn,
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
