use std::collections::BTreeSet;

use crate::{executor::{
        semantic_analysis::hm::types::Type,
        value::{Value },
    }, parsing::{Span, }};
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum SymbolError<'a> {
    #[error("invalid symbol:\n{}:{}:{}\n\t{name}", .span.extra, .span.location_line(), .span.get_utf8_column())]
    InvalidSymbol { name: &'a str, span: Span<'a> },
    #[error("invalid fn:\n{}:{}:{}\n\t{name}", .span.extra, .span.location_line(), .span.get_utf8_column())]
    InvalidFn { name: &'a str, span: Span<'a> },
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum CompileError<'a> {
    #[error("symbol error: {0}")]
    Symbol(SymbolError<'a>),
    #[error("type error: {0}")]
    Type(TypeError<'a>),
    #[error("extern error: invalid extern\n{}:{}:{}\n\t{span}", .span.extra, .span.location_line(), .span.get_utf8_column())]
    InvalidExtern {
        name: &'a str,
        span: Span<'a>,
    }
}


impl<'a> From<SymbolError<'a>> for CompileError<'a> {
    fn from(inner: SymbolError<'a>) -> Self {
        Self::Symbol(inner)
    }
}

impl<'a> From<TypeError<'a>> for CompileError<'a> {
    fn from(inner: TypeError<'a>) -> Self {
        Self::Type(inner)
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeError<'a> {
    #[error("invalid type\n{}:{}:{}\n\texpected: {expected}\n\tfound: {found} ", .span.extra, .span.location_line(), .span.get_utf8_column())]
    InvalidType {
        expected: Type<'a>,
        found: Type<'a>,
        span: Span<'a>,
    },
    #[error("multiple array types\n{}:{}:{}\n\tfound: {}", .span.extra, .span.location_line(), .span.get_utf8_column(), render_array(.found))]
    InvalidArray {
        found: BTreeSet<Type<'a>>,
        span: Span<'a>,
    },
    #[error("infinite type\n{}:{}:{}\n\t'{left}' <- '{right}'", .span.extra, .span.location_line(), .span.get_utf8_column())]
    InfiniteType {
        left: Type<'a>,
        right: Type<'a>,
        span: Span<'a>,
    },
    #[error("ambiguous type\n{}:{}:{}\n\t{ty}\n\t{}",
        .ty.span().extra, 
        .ty.span().location_line(), 
        .ty.span().get_utf8_column(), 
        .ty.span().fragment()
    )]
    UninferredType { ty: Type<'a> },
}

fn render_array<'a>(found: &BTreeSet<Type<'a>>) -> String {
    found.into_iter().map(|ty| format!("{}", ty)).intersperse(format!(", ")).collect()
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum RuntimeError {
    #[error("runtime error: invalid type\n\tfound {found}\n\texpected {expected}")]
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
