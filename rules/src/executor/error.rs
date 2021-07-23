use crate::{
    executor::{semantic_analysis::hm::types::Type, value::Value},
    parsing::{ParseError, Span},
};
use arcstr::Substr;
use std::fmt::Display;
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
    #[error("{}", make_error_message("invalid extern", "invalid extern", .span))]
    InvalidExtern { name: Substr, span: Span },
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("symbol error: {0}")]
    Symbol(#[from] SymbolError),
    #[error("type error: {0}")]
    Type(#[from] TypeError),

    #[error("file system error: {0}")]
    FileSystem(#[from] std::io::Error),

    #[error("UTF8 error: {0}")]
    UTF8(#[from] std::string::FromUtf8Error),

    // TODO: format a parse error properly
    #[error("parse error: {0}")]
    Parse(#[from] ParseError),

    #[error("{}", render_list(&.0))]
    List(Vec<CompileError>),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeError {
    #[error("{}", make_error_message("mismatched types", &format!("expected `{}`, found `{}`", .expected, .found), .found.span()))]
    InvalidType { expected: Type, found: Type },

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
    #[error("bytecode should not try to load label directly: '{0}")]
    LoadLabel(usize),
    #[error("missing heap value name: {0}")]
    MissingHeapValue(String),
    #[error("invalid fn name \"{0}\"")]
    InvalidFn(String),
    #[error("invalid call-dynamic, found: {0}, expected: RustFn|Label")]
    InvalidCallDynamic(Value),
    #[error("found an empty array when an array with at least 1 element was expected")]
    EmptyArray,
}
