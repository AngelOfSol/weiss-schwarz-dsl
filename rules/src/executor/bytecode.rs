use std::fmt::Display;

use crate::executor::value::Value;

pub type ExecutableBytecode = InternalBytecode<usize>;
pub type LabeledBytecode = InternalBytecode<String>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InternalBytecode<LabelType> {
    Print,
    Call(String),
    CallDynamic,
    Load(Value),
    LoadLabel(LabelType),
    Jump(LabelType),
    JumpIf(LabelType),
    Store(String),
    LoadRef(String),
    Unload(String),
    /// NO-OP, specifically added for debugging purposes
    Label(String),
    Return,
}

impl<LabelType: Display> Display for InternalBytecode<LabelType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InternalBytecode::Print => write!(f, "print"),
            InternalBytecode::Call(name) => write!(f, "call {}", name),
            InternalBytecode::Load(value) => write!(f, "load {}", value),
            InternalBytecode::LoadLabel(value) => write!(f, "load {}", value),
            InternalBytecode::Jump(label) => write!(f, "jump '{}", label),
            InternalBytecode::JumpIf(label) => write!(f, "jump-if-true '{}", label),
            InternalBytecode::Label(label) => write!(f, "'{}:", label),
            InternalBytecode::Return => write!(f, "return"),
            InternalBytecode::Store(name) => write!(f, "store @{}", name),
            InternalBytecode::LoadRef(name) => write!(f, "load-ref @{}", name),
            InternalBytecode::Unload(name) => write!(f, "unload @{}", name),
            InternalBytecode::CallDynamic => write!(f, "call-dynamic"),
        }
    }
}
