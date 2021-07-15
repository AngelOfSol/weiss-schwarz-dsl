use nom::{
    bytes::complete::tag,
    character::complete::{multispace1, one_of},
    combinator::{map, not, recognize, value},
    multi::{many0, many1},
    sequence::{delimited, pair},
    IResult,
};

use crate::parsing::Span;

fn from_span(span: Span) -> &str {
    *span.fragment()
}

pub fn identifier(input: Span) -> IResult<Span, &str> {
    map(
        recognize(pair(
            one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
            many0(one_of(
                "-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
            )),
        )),
        from_span,
    )(input)
}

pub fn number(input: Span) -> IResult<Span, i32> {
    map(recognize(many1(one_of("0123456789"))), |data: Span| {
        data.fragment().parse().unwrap()
    })(input)
}

pub fn whitespace(input: Span) -> IResult<Span, Span> {
    multispace1(input)
}

pub fn raw_string(input: Span) -> IResult<Span, &str> {
    map(
        delimited(tag("\""), recognize(many0(not(tag("\"")))), tag("\"")),
        from_span,
    )(input)
}

pub fn backtick(input: Span) -> IResult<Span, ()> {
    value((), tag("`"))(input)
}

pub fn arrow(input: Span) -> IResult<Span, ()> {
    value((), tag("->"))(input)
}

pub fn open(input: Span) -> IResult<Span, ()> {
    value((), tag("("))(input)
}

pub fn close(input: Span) -> IResult<Span, ()> {
    value((), tag(")"))(input)
}

pub fn open_array(input: Span) -> IResult<Span, ()> {
    value((), tag("["))(input)
}

pub fn close_array(input: Span) -> IResult<Span, ()> {
    value((), tag("]"))(input)
}

pub fn ascribe(input: Span) -> IResult<Span, ()> {
    value((), tag(":"))(input)
}

pub fn traits(input: Span) -> IResult<Span, ()> {
    value((), tag("#"))(input)
}
