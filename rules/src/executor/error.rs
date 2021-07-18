use std::collections::BTreeSet;

use crate::{
    executor::{semantic_analysis::hm::types::Type, value::Value},
    parsing::Span,
};
use thiserror::Error;

fn make_file_indicator(span: Span) -> String {
    format!(
        " --> {filename}:{line}:{column}",
        filename = span.extra,
        line = span.location_line(),
        column = span.get_utf8_column()
    )
}

fn make_line(span: Span) -> String {
    format!(
        "{}",
        std::str::from_utf8(span.get_line_beginning()).unwrap()
    )
}
fn make_caret(span: Span) -> String {
    format!(
        "{blank:>column$}{blank:^>len$}",
        blank = "",
        column = span.get_utf8_column() - 1,
        len = span.fragment().trim().len()
    )
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum SymbolError<'a> {
    #[error("{error_message}\n\
        {file_indicator}\n  \
        |\n  \
        | {fragment}\n  \
        | {carets} {error_message}",
        file_indicator = make_file_indicator(*.span),
        fragment = make_line(*.span),
        carets = make_caret(*.span),
        error_message = "invalid symbol"
    )]
    InvalidSymbol { name: &'a str, span: Span<'a> },
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum CompileError<'a> {
    #[error("symbol error: {0}")]
    Symbol(SymbolError<'a>),
    #[error("type error: {0}")]
    Type(TypeError<'a>),
    #[error("{error_message}:\n\
        {file_indicator}\n  \
        |\n  \
        | {fragment}\n  \
        | {carets} {error_message}",
        file_indicator = make_file_indicator(*.span),
        fragment = make_line(*.span),
        carets = make_caret(*.span),
        error_message = "invalid extern"
    )]
    InvalidExtern { name: &'a str, span: Span<'a> },
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
    #[error("{error_name}\n\
        {file_indicator}\n  \
        |\n  \
        | {fragment}\n  \
        | {carets} expected: {expected}, found: {found}",
        file_indicator = make_file_indicator(*.span),
        fragment = make_line(*.span),
        carets = make_caret(*.span),
        error_name = "mismatched types"
    )]
    InvalidType {
        expected: Type<'a>,
        found: Type<'a>,
        span: Span<'a>,
    },

    #[error("{error_name}\n\
        {file_indicator}\n  \
        |\n  \
        | {fragment}\n  \
        | {carets} found multiple types: {rendered}",
        file_indicator = make_file_indicator(*.span),
        fragment = make_line(*.span),
        carets = make_caret(*.span),
        rendered = render_array(.found),
        error_name = "invalid array"
    )]
    InvalidArray {
        found: BTreeSet<Type<'a>>,
        span: Span<'a>,
    },
    #[error("{error_name}\n\
        {file_indicator}\n  \
        |\n  \
        | {fragment}\n  \
        | {carets} `{left}` <- `{right}`",
        file_indicator = make_file_indicator(*.span),
        fragment = make_line(*.span),
        carets = make_caret(*.span),
        error_name = "infinite type"
    )]
    InfiniteType {
        left: Type<'a>,
        right: Type<'a>,
        span: Span<'a>,
    },
    #[error("{error_message}\n\
        {file_indicator}\n  \
        |\n  \
        | {fragment}\n  \
        | {carets} {error_message}",
        file_indicator = make_file_indicator(*.ty.span()),
        fragment = make_line(*.ty.span()),
        carets = make_caret(*.ty.span()),
        error_message = "ambiguous type"
    )]
    UninferredType { ty: Type<'a> },
}

fn render_array<'a>(found: &BTreeSet<Type<'a>>) -> String {
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
    #[error("missing heap value idx: {0}")]
    MissingHeapValue(usize),
    #[error("invalid fn name \"{0}\"")]
    InvalidFn(String),
}
