pub mod bytecode;
pub mod code_generation;
pub mod error;
pub mod rust_funcs;
pub mod semantic_analysis;
pub mod value;

use crate::executor::{
    bytecode::ExecutableBytecode, code_generation::Generated, error::RuntimeError, value::Label,
};
use crate::{
    executor::value::{Value, ValueFrom},
    model::Game,
};
use arcstr::Substr;
use lazy_static::lazy_static;
use maplit::hashmap;
use std::collections::HashMap;

#[derive(Default)]
pub struct Executor {
    pub stack: Stack,
    pub heap: Heap,
    pub ip: usize,
    pub ip_stack: Vec<usize>,
}

#[derive(Default, Debug)]
pub struct Stack {
    stack: Vec<Value>,
}

#[derive(Debug)]
pub struct Heap {
    heap: HashMap<Substr, Vec<Value>>,
}

impl Default for Heap {
    fn default() -> Self {
        let mut heap = Self {
            heap: HashMap::new(),
        };
        for f in RUST_FN.keys() {
            heap.store(Substr::from(*f), Value::RustFn(*f));
        }
        heap
    }
}

impl Executor {
    pub fn advance(
        &mut self,
        generated: &Generated,
        game: &mut Game,
    ) -> Result<Option<String>, RuntimeError> {
        let code = generated
            .executable
            .get(self.ip)
            .ok_or(RuntimeError::InvalidBytecodeOffset)?;

        let mut ret = None;

        let advance_intruction_pointer = match code {
            ExecutableBytecode::Print => {
                let value = self.stack.pop_any()?;

                ret = Some(format!("{}", &value));
                self.stack.push_any(value);

                true
            }
            ExecutableBytecode::Call(name) => {
                if let Some(func) = RUST_FN.get(name.as_str()) {
                    func(self, game)?;
                    true
                } else if let Some(label) = generated.labels.get(name) {
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
            ExecutableBytecode::Unload => {
                let _ = self.stack.pop_any()?;

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
            ExecutableBytecode::UnloadRef(idx) => {
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
                            return Err(RuntimeError::InvalidFn(name.into()));
                        }
                        true
                    }
                    value => return Err(RuntimeError::InvalidCallDynamic(value)),
                }
            }
            ExecutableBytecode::MakeArray => {
                let len = self.stack.pop::<i32>()?;
                let mut array = vec![];
                for _ in 0..len {
                    array.insert(0, self.stack.pop_any()?);
                }
                self.stack.push(Value::from(array));
                true
            }
        };

        if advance_intruction_pointer {
            self.ip += 1;
        }

        Ok(ret)
    }
    pub fn reset(&mut self) {
        self.stack.stack.clear();
        self.ip = 0;
        self.ip_stack.clear();
        self.heap.clear();
    }
}

impl Heap {
    pub fn clear(&mut self) {
        *self = Self::default();
    }
    pub fn store(&mut self, key: Substr, value: Value) {
        let data = self.heap.entry(key).or_default();
        data.push(value);
    }

    pub fn load(&mut self, key: &str) -> Result<Value, RuntimeError> {
        if let Some(value) = self.heap.get(key).and_then(|internal| internal.last()) {
            Ok(value.clone())
        } else {
            Err(RuntimeError::MissingHeapValue(key.into()))
        }
    }
    pub fn unload(&mut self, key: &str) -> Result<(), RuntimeError> {
        if let Some(_) = self.heap.get_mut(key).and_then(|inner| inner.pop()) {
            Ok(())
        } else {
            Err(RuntimeError::MissingHeapValue(key.into()))
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = (&Substr, &Vec<Value>)> {
        self.heap.iter()
    }
}

impl Stack {
    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.stack.iter()
    }

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
        "move!" => rust_funcs::move_card as ExecutorFn,
        "some" => rust_funcs::some as ExecutorFn,
        "or_default" => rust_funcs::or_default as ExecutorFn,
        "+" => rust_funcs::add as ExecutorFn,
        "-" => rust_funcs::sub as ExecutorFn,
        "==" => rust_funcs::eq as ExecutorFn,
        "head" => rust_funcs::head as ExecutorFn,
        "tail" => rust_funcs::tail as ExecutorFn,
        "cons" => rust_funcs::cons as ExecutorFn,
        "is_empty?" => rust_funcs::is_empty as ExecutorFn,
    };
}
