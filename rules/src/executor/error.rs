use crate::{
    executor::value::{Value, ValueType},
    parsing::SexprValue,
};
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum CompileError<'a> {
    #[error("expected an s-expr, found: {0:?}")]
    ExpectedSexpr(&'a SexprValue<'a>),
    #[error("invalid argument count for {target}, expected {expected}, found {found}")]
    InvalidArgumentAmount {
        target: &'a str,
        expected: usize,
        found: usize,
    },
    #[error("invalid symbol: {0}")]
    InvalidSymbol(&'a str),
    #[error("invalid fn: {0}")]
    InvalidFn(&'a str),
    #[error("invalid type for {target}, expected {expected}, found {found}: \"{value}\"")]
    InvalidType {
        target: &'a str,
        expected: ValueType,
        found: ValueType,
        value: &'a SexprValue<'a>,
    },
    #[error("invalid fn {target}, found {found}")]
    InvalidFnType { target: &'a str, found: ValueType },
    #[error("invalid condition for 'if', expected bool, found {found}: \"{value}\"")]
    InvalidCondition {
        found: ValueType,
        value: &'a SexprValue<'a>,
    },
    #[error("incompatible types for 'if', true branch returns {true_type}, but false returns {false_type}, \"{value}\"")]
    IncompatibleTypes {
        true_type: ValueType,
        false_type: ValueType,
        value: &'a SexprValue<'a>,
    },
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum RuntimeError {
    #[error("invalid type, found {found}, expected {expected}")]
    InvalidType { found: Value, expected: String },
    #[error("attempted to pop an argument on an empty stack")]
    EmptyStack,
    #[error("no instruction pointer")]
    NoInstructionPointer,
    #[error("invalid instruction pointer: invalid code table")]
    InvalidCodeTable,
    #[error("invalid instruction pointer: invalid bytecode offset")]
    InvalidBytecodeOffset,
    #[error("code isn't finished running")]
    Unfinished(String),
    #[error("missing heap value idx: {0}")]
    MissingHeapValue(usize),
    #[error("invalid fn name \"{0}\"")]
    InvalidFn(String),
}
