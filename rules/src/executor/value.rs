use std::{convert::TryFrom, fmt::Display};

use crate::{
    executor::{error::RuntimeError, FnTypeInfo},
    model::{CardId, ZoneId},
    parsing::Sexpr,
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
    ArrayLength(usize),
    Unit,
    Zone(ZoneId),
    CardId(CardId),
    Bool(bool),
    Some(Box<Value>),
    Label(Label),
    None,
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum ValueType {
    Integer,
    Unit,
    Zone,
    CardId,
    Bool,
    Inferred,
    Label,
    Option(Box<ValueType>),
    Array(Box<ValueType>),
    Fn(Box<FnTypeInfo>),
}

impl ValueType {
    pub fn as_fn(&self) -> Option<&FnTypeInfo> {
        if let Self::Fn(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn matches(&self, rhs: &Self) -> bool {
        if self == &ValueType::Option(Box::new(ValueType::Unit))
            && matches!(rhs, ValueType::Option(_))
        {
            true
        } else if rhs == &ValueType::Option(Box::new(ValueType::Unit))
            && matches!(self, ValueType::Option(_))
        {
            true
        } else {
            self == rhs
        }
    }
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Integer => write!(f, "i32"),
            ValueType::Unit => write!(f, "()"),
            ValueType::Zone => write!(f, "zone"),
            ValueType::CardId => write!(f, "card_id"),
            ValueType::Bool => write!(f, "bool"),
            ValueType::Array(ty) => write!(f, "[{}]", ty),
            ValueType::Fn(fn_type) => write!(f, "{}", fn_type),
            ValueType::Option(ty) => write!(f, "{}?", ty),
            ValueType::Inferred => write!(f, "inferred"),
            ValueType::Label => write!(f, "label",),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "{}: i32", value),
            Value::ArrayLength(value) => write!(f, "{}: array_length", value),
            Value::Unit => write!(f, "(): unit"),
            Value::Zone(value) => write!(f, "{}: zone", value),
            Value::CardId(value) => write!(f, "{}: card", value.0),
            Value::Bool(value) => write!(f, "{}: bool", value),
            Value::None => write!(f, "none: ?"),
            Value::Some(inner) => write!(f, "some {}?", inner),
            Value::Label(inner) => write!(f, "label@{}", inner.name),
        }
    }
}

impl<'a> TryFrom<Sexpr<'a>> for Value {
    type Error = Sexpr<'a>;

    fn try_from(value: Sexpr<'a>) -> Result<Self, Self::Error> {
        match value {
            Sexpr::Integer(integer, ..) => Ok(Value::Integer(integer)),
            Sexpr::Zone(zone, ..) => Ok(Value::Zone(zone)),
            Sexpr::Unit(..) => Ok(Value::Unit),
            Sexpr::Bool(value, ..) => Ok(Value::Bool(value)),
            Sexpr::None(..) => Ok(Value::None),
            x @ Sexpr::Array { .. }
            | x @ Sexpr::Symbol(_, ..)
            | x @ Sexpr::Let { .. }
            | x @ Sexpr::If { .. }
            | x @ Sexpr::Eval { .. }
            | x @ Sexpr::Fn { .. }
            | x @ Sexpr::Seq { .. } => Err(x),
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
impl_variant!(ArrayLength, usize);
impl_variant!(Label, Label);
impl_variant!(Zone, ZoneId);
impl_variant!(CardId, CardId);
