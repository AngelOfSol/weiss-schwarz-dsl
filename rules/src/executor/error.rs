use std::{collections::BTreeSet, fmt::Display};

use crate::{
    executor::{semantic_analysis::hm::types::Type, value::Value},
    parsing::Span,
};
use arcstr::Substr;
use thiserror::Error;

pub fn make_error_message(error_name: &str, error_message: &str, span: &Span) -> String {
    let context = span
        .parent()
        .lines()
        .nth(span.location_line() as usize - 1)
        .unwrap();

    format!(
        "{error_name}\n\
            {file_indicator}\n\
            {line_preamble}\n\
            {line_preamble}{fragment}\n\
            {line_preamble}{carets} {error_message}\n\
        ",
        line_preamble = "  | ",
        file_indicator = make_file_indicator(span),
        carets = make_caret(span),
        error_name = error_name,
        fragment = context,
        error_message = error_message,
    )
}

fn make_file_indicator(span: &Span) -> String {
    format!(
        " --> {filename}:{line}:{column}",
        filename = span.extra,
        line = span.location_line(),
        column = span.get_utf8_column()
    )
}

fn make_line(span: &Span) -> String {
    format!(
        "{}",
        std::str::from_utf8(span.get_line_beginning()).unwrap()
    )
}
fn make_caret(span: &Span) -> String {
    format!(
        "{blank:>column$}{blank:^>len$}",
        blank = "",
        column = span.get_utf8_column() - 1,
        len = span.fragment().trim().len()
    )
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum SymbolError {
    #[error("{}", make_error_message("invalid symbol", "invalid_symbol", .span))]
    InvalidSymbol { name: Substr, span: Span },
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum CompileError {
    #[error("symbol error: {0}")]
    Symbol(SymbolError),
    #[error("type error: {0}")]
    Type(TypeError),

    #[error("{}", make_error_message("invalid extern", "invalid extern", .span))]
    InvalidExtern { name: Substr, span: Span },

    #[error("{}", render_list(&.0))]
    List(Vec<CompileError>),
}

impl From<SymbolError> for CompileError {
    fn from(inner: SymbolError) -> Self {
        Self::Symbol(inner)
    }
}

impl From<TypeError> for CompileError {
    fn from(inner: TypeError) -> Self {
        Self::Type(inner)
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeError {
    #[error("{}", make_error_message("mismatched types", &format!("expected: {}, found: {}", .expected, .found), .found.span()))]
    InvalidType { expected: Type, found: Type },

    #[error("{}", make_error_message("invalid array", &format!("found multiple types: {}", render_array(.found)), .span))]
    InvalidArray { found: BTreeSet<Type>, span: Span },

    #[error("{}", make_error_message("infinite type", &format!("`{}` <- `{}`", .left, .right), .span))]
    InfiniteType { left: Type, right: Type, span: Span },

    #[error("{}", make_error_message("ambiguous type", "ambiguous type", .ty.span()))]
    UninferredType { ty: Type },
}

fn render_list<T: Display>(list: &[T]) -> String {
    list.iter()
        .map(ToString::to_string)
        .intersperse("\n".to_string())
        .collect()
}

fn render_array(found: &BTreeSet<Type>) -> String {
    found
        .into_iter()
        .map(|ty| format!("{}", ty))
        .intersperse(format!(", "))
        .collect()
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
    #[error("bytecode should not try to load label directly: '{0}")]
    LoadLabel(usize),
    #[error("missing heap value name: {0}")]
    MissingHeapValue(String),
    #[error("invalid fn name \"{0}\"")]
    InvalidFn(String),
    #[error("invalid call-dynamic, found: {0}, expected: RustFn|Label")]
    InvalidCallDynamic(Value),
}
