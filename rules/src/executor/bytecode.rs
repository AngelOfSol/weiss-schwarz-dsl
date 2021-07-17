use std::fmt::Display;

use crate::executor::value::Value;

pub type Bytecode = InternalBytecode<usize>;
pub type LabeledBytecode = InternalBytecode<String>;

impl LabeledBytecode {
    pub fn print() -> Self {
        InternalBytecode::Print
    }
    pub fn ret() -> Self {
        InternalBytecode::Return
    }
    pub fn call(target: String) -> Self {
        InternalBytecode::Call(target)
    }
    pub fn load(target: Value) -> Self {
        InternalBytecode::Load(target)
    }
    pub fn jump(target: String) -> Self {
        InternalBytecode::Jump(target)
    }
    pub fn jump_if(target: String) -> Self {
        InternalBytecode::JumpIf(target)
    }

    pub fn label(label: String) -> Self {
        InternalBytecode::Label(label)
    }

    pub fn load_ref(binding: String) -> Self {
        InternalBytecode::LoadRef(binding)
    }
    pub fn store(binding: String) -> Self {
        InternalBytecode::Store(binding)
    }
    pub fn unload(binding: String) -> Self {
        InternalBytecode::Unload(binding)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InternalBytecode<LabelType> {
    Print,
    Call(String),
    CallDynamic,
    Load(Value),
    LoadLabel(LabelType),
    Jump(LabelType),
    JumpIf(LabelType),
    Store(LabelType),
    LoadRef(LabelType),
    Unload(LabelType),
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
