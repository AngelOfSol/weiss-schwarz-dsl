use std::fmt::Display;

use crate::{
    executor::error::RuntimeError,
    model::{CardId, ZoneId},
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct Label {
    pub name: String,
    pub ip: usize,
}

pub trait ValueFrom: Sized {
    fn try_from(value: Value) -> Result<Self, RuntimeError>;
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum Value {
    Integer(i32),
    Array(Vec<Value>),
    Unit,
    Zone(ZoneId),
    CardId(CardId),
    Bool(bool),
    Some(Box<Value>),
    Label(Label),
    RustFn(&'static str),
    None,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "{}: i32", value),
            Value::Array(value) => write!(
                f,
                "[{}]",
                value
                    .iter()
                    .map(ToString::to_string)
                    .intersperse(", ".to_string())
                    .collect::<String>()
            ),
            Value::Unit => write!(f, "(): unit"),
            Value::Zone(value) => write!(f, "{}: zone", value),
            Value::CardId(value) => write!(f, "{}: card", value.0),
            Value::Bool(value) => write!(f, "{}: bool", value),
            Value::None => write!(f, "none: ?"),
            Value::Some(inner) => write!(f, "some {}?", inner),
            Value::Label(inner) => write!(f, "label@{}", inner.name),
            Value::RustFn(inner) => write!(f, "rust@{}", inner),
        }
    }
}

macro_rules! impl_variant {
    ($variant_name:ident, $impl_type:ty) => {
        impl ValueFrom for $impl_type {
            fn try_from(value: Value) -> Result<Self, RuntimeError> {
                if let Value::$variant_name(v) = value {
                    Ok(v)
                } else {
                    Err(RuntimeError::InvalidType {
                        found: value,
                        expected: stringify!($impl_type).to_string(),
                    })
                }
            }
        }

        impl From<$impl_type> for Value {
            fn from(v: $impl_type) -> Self {
                Self::$variant_name(v)
            }
        }
    };
}

impl_variant!(Bool, bool);
impl_variant!(Integer, i32);
impl_variant!(Array, Vec<Value>);
impl_variant!(Label, Label);
impl_variant!(Zone, ZoneId);
impl_variant!(CardId, CardId);
