use std::fmt::Display;

use arcstr::Substr;

use crate::executor::value::Value;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExecutableBytecode {
    Print,
    Call(String),
    CallDynamic,
    Load(Value),
    Unload,
    Jump(usize),
    JumpIf(usize),
    Store(String),
    LoadRef(String),
    UnloadRef(String),
    /// NO-OP, specifically added for debugging purposes
    Label(String),
    Return,
    MakeArray,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LabeledBytecode {
    Print,
    Call(Substr),
    CallDynamic,
    Load(Value),
    Unload,
    LoadLabel(Substr),
    Jump(Substr),
    JumpIf(Substr),
    Store(Substr),
    LoadRef(Substr),
    UnloadRef(Substr),
    /// NO-OP, specifically added for debugging purposes
    Label(Substr),
    Return,
    MakeArray,
}

impl Display for LabeledBytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LabeledBytecode::Print => write!(f, "print"),
            LabeledBytecode::Call(name) => write!(f, "call {}", name),
            LabeledBytecode::Load(value) => write!(f, "load {}", value),
            LabeledBytecode::Unload => write!(f, "unload"),
            LabeledBytecode::LoadLabel(value) => write!(f, "load {}", value),
            LabeledBytecode::Jump(label) => write!(f, "jump '{}", label),
            LabeledBytecode::JumpIf(label) => write!(f, "jump-if-true '{}", label),
            LabeledBytecode::Label(label) => write!(f, "'{}:", label),
            LabeledBytecode::Return => write!(f, "return"),
            LabeledBytecode::Store(name) => write!(f, "store @{}", name),
            LabeledBytecode::LoadRef(name) => write!(f, "load-ref @{}", name),
            LabeledBytecode::UnloadRef(name) => write!(f, "unload @{}", name),
            LabeledBytecode::CallDynamic => write!(f, "call-dynamic"),
            LabeledBytecode::MakeArray => write!(f, "make-array"),
        }
    }
}
