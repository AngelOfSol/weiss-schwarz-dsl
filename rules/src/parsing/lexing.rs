use nom::{
    bytes::complete::tag,
    character::complete::{multispace1, one_of},
    combinator::{map, not, recognize, value},
    multi::{many0, many1},
    sequence::{delimited, pair},
    IResult,
};

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        many0(one_of(
            "-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
        )),
    ))(input)
}

pub fn number(input: &str) -> IResult<&str, i32> {
    map(recognize(many1(one_of("0123456789"))), |data: &str| {
        data.parse().unwrap()
    })(input)
}

pub fn whitespace(input: &str) -> IResult<&str, &str> {
    multispace1(input)
}

pub fn raw_string(input: &str) -> IResult<&str, &str> {
    delimited(tag("\""), recognize(many0(not(tag("\"")))), tag("\""))(input)
}

pub fn backtick(input: &str) -> IResult<&str, ()> {
    value((), tag("`"))(input)
}

pub fn arrow(input: &str) -> IResult<&str, ()> {
    value((), tag("->"))(input)
}

pub fn open(input: &str) -> IResult<&str, ()> {
    value((), tag("("))(input)
}

pub fn close(input: &str) -> IResult<&str, ()> {
    value((), tag(")"))(input)
}

pub fn open_array(input: &str) -> IResult<&str, ()> {
    value((), tag("["))(input)
}

pub fn close_array(input: &str) -> IResult<&str, ()> {
    value((), tag("]"))(input)
}

pub fn ascribe(input: &str) -> IResult<&str, ()> {
    value((), tag(":"))(input)
}

pub fn traits(input: &str) -> IResult<&str, ()> {
    value((), tag("#"))(input)
}
