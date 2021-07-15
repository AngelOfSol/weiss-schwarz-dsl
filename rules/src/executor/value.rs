use std::{convert::TryFrom, fmt::Display};

use crate::{
    executor::{error::RuntimeError, FnTypeInfo},
    model::{CardId, ZoneId},
    parsing::SexprValue,
};
use serde::{Deserialize, Serialize};

pub trait ValueFrom: Sized {
    fn try_from(value: Value) -> Result<Self, RuntimeError>;
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum Value {
    Integer(i32),
    ArrayLength(usize),
    Unit(()),
    Zone(ZoneId),
    CardId(CardId),
    Bool(bool),
    Some(Box<Value>),
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
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "{}: i32", value),
            Value::ArrayLength(value) => write!(f, "{}: array_length", value),
            Value::Unit(_) => write!(f, "(): unit"),
            Value::Zone(value) => write!(f, "{}: zone", value),
            Value::CardId(value) => write!(f, "{}: card", value.0),
            Value::Bool(value) => write!(f, "{}: bool", value),
            Value::None => write!(f, "none: ?"),
            Value::Some(inner) => write!(f, "some {}?", inner),
        }
    }
}

impl<'a> TryFrom<SexprValue<'a>> for Value {
    type Error = SexprValue<'a>;

    fn try_from(value: SexprValue<'a>) -> Result<Self, Self::Error> {
        match value {
            SexprValue::Integer(integer) => Ok(Value::Integer(integer)),
            SexprValue::Zone(zone) => Ok(Value::Zone(zone)),
            SexprValue::Unit(()) => Ok(Value::Unit(())),
            SexprValue::Bool(value) => Ok(Value::Bool(value)),
            SexprValue::None => Ok(Value::None),
            x @ SexprValue::Array { .. }
            | x @ SexprValue::Symbol(_, ..)
            | x @ SexprValue::Let { .. }
            | x @ SexprValue::If { .. }
            | x @ SexprValue::Sexpr(_)
            | x @ SexprValue::Fn { .. } => Err(x),
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

impl_variant!(Unit, ());
impl_variant!(Bool, bool);
impl_variant!(Integer, i32);
impl_variant!(ArrayLength, usize);
impl_variant!(Zone, ZoneId);
impl_variant!(CardId, CardId);
