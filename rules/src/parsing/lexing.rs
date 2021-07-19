use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{multispace0, one_of},
    combinator::{cut, map, recognize, value},
    error::{context, ParseError},
    multi::{many0, many1},
    sequence::{delimited, pair},
    AsChar, IResult as NomResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
};

use crate::parsing::Span;

pub type IResult<I, O, E = nom::error::VerboseError<I>> = NomResult<I, O, E>;

macro_rules! contextual_tag {
    ($value:expr) => {
        context(concat!("expected ", stringify!($value)), tag($value))
    };
}

fn from_span(span: Span) -> &str {
    *span.fragment()
}

pub fn ws<F, I, O, E: ParseError<I>>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: FnMut(I) -> IResult<I, O, E>,
    I: InputLength + InputIter + InputTake + Clone + InputTakeAtPosition,
    <I as InputIter>::Item: AsChar + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(multispace0, inner, multispace0)
}

pub fn identifier(input: Span) -> IResult<Span, &str> {
    map(
        ws(recognize(pair(
            one_of("-+=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
            many0(one_of(
                "-+=_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
            )),
        ))),
        from_span,
    )(input)
}

pub fn number(input: Span) -> IResult<Span, i32> {
    map(ws(recognize(many1(one_of("0123456789")))), |data: Span| {
        data.fragment().parse().unwrap()
    })(input)
}

pub fn raw_string(input: Span) -> IResult<Span, &str> {
    map(
        ws(delimited(
            contextual_tag!("\""),
            recognize(take_until("\"")),
            contextual_tag!("\""),
        )),
        from_span,
    )(input)
}

pub fn arrow(input: Span) -> IResult<Span, ()> {
    value((), ws(contextual_tag!("->")))(input)
}

pub fn open(input: Span) -> IResult<Span, ()> {
    value((), ws(contextual_tag!("(")))(input)
}

pub fn close_type_variables(input: Span) -> IResult<Span, ()> {
    value((), ws(contextual_tag!(">")))(input)
}

pub fn open_type_variables(input: Span) -> IResult<Span, ()> {
    value((), ws(contextual_tag!("<")))(input)
}

pub fn close(input: Span) -> IResult<Span, ()> {
    cut(value((), ws(contextual_tag!(")"))))(input)
}

pub fn open_array(input: Span) -> IResult<Span, ()> {
    value((), ws(contextual_tag!("[")))(input)
}

pub fn close_array(input: Span) -> IResult<Span, ()> {
    value((), ws(contextual_tag!("]")))(input)
}

pub fn ascribe(input: Span) -> IResult<Span, ()> {
    value((), ws(contextual_tag!(":")))(input)
}
