use std::fmt::Display;

use crate::executor::value::Value;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExecutableBytecode {
    Print,
    Call(String),
    CallDynamic,
    Load(Value),
    Jump(usize),
    JumpIf(usize),
    Store(String),
    LoadRef(String),
    Unload(String),
    /// NO-OP, specifically added for debugging purposes
    Label(String),
    Return,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LabeledBytecode {
    Print,
    Call(String),
    CallDynamic,
    Load(Value),
    LoadLabel(String),
    Jump(String),
    JumpIf(String),
    Store(String),
    LoadRef(String),
    Unload(String),
    /// NO-OP, specifically added for debugging purposes
    Label(String),
    Return,
}

impl Display for LabeledBytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LabeledBytecode::Print => write!(f, "print"),
            LabeledBytecode::Call(name) => write!(f, "call {}", name),
            LabeledBytecode::Load(value) => write!(f, "load {}", value),
            LabeledBytecode::LoadLabel(value) => write!(f, "load {}", value),
            LabeledBytecode::Jump(label) => write!(f, "jump '{}", label),
            LabeledBytecode::JumpIf(label) => write!(f, "jump-if-true '{}", label),
            LabeledBytecode::Label(label) => write!(f, "'{}:", label),
            LabeledBytecode::Return => write!(f, "return"),
            LabeledBytecode::Store(name) => write!(f, "store @{}", name),
            LabeledBytecode::LoadRef(name) => write!(f, "load-ref @{}", name),
            LabeledBytecode::Unload(name) => write!(f, "unload @{}", name),
            LabeledBytecode::CallDynamic => write!(f, "call-dynamic"),
        }
    }
}
