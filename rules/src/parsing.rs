pub mod lexing;

use std::fmt::Display;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{consumed, map, map_opt, opt, value},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use crate::executor::{value::ValueType, FnTypeInfo};
use crate::model::ZoneId;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span<'a> {
    data: &'a str,
}

pub fn parse_sexpr(input: &str) -> IResult<&str, Sexpr> {
    map(
        consumed(delimited(
            lexing::open,
            tuple((
                lexing::identifier,
                many0(preceded(lexing::whitespace, parse_sexpr_value)),
                opt(lexing::whitespace),
            )),
            lexing::close,
        )),
        |(span, (symbol, arguments, _))| Sexpr {
            target: symbol,
            arguments,
            span: Span { data: span },
        },
    )(input)
}

pub fn parse_array(input: &str) -> IResult<&str, SexprValue> {
    map(
        consumed(delimited(
            lexing::open_array,
            tuple((
                parse_sexpr_value,
                many0(preceded(lexing::whitespace, parse_sexpr_value)),
                opt(lexing::whitespace),
            )),
            lexing::close_array,
        )),
        |(span, (first, mut rest, _))| {
            rest.insert(0, first);
            SexprValue::Array {
                values: rest,
                span: Span { data: span },
            }
        },
    )(input)
}

pub fn parse_let(input: &str) -> IResult<&str, SexprValue> {
    map(
        consumed(delimited(
            lexing::open,
            tuple((
                tag("let"),
                lexing::whitespace,
                delimited(
                    lexing::open,
                    many1(delimited(
                        lexing::open,
                        tuple((
                            lexing::identifier,
                            preceded(lexing::whitespace, parse_sexpr_value),
                        )),
                        lexing::close,
                    )),
                    lexing::close,
                ),
                preceded(lexing::whitespace, parse_sexpr_value),
            )),
            lexing::close,
        )),
        |(span, (_, _, bindings, expr))| SexprValue::Let {
            bindings,
            expr: Box::new(expr),
            span: Span { data: span },
        },
    )(input)
}

pub fn parse_fn(input: &str) -> IResult<&str, SexprValue> {
    map(
        consumed(delimited(
            lexing::open,
            tuple((
                tag("fn"),
                lexing::whitespace,
                delimited(
                    lexing::open,
                    many0(preceded(
                        opt(lexing::whitespace),
                        nom::sequence::pair(
                            lexing::identifier,
                            map(
                                opt(preceded(
                                    tuple((
                                        opt(lexing::whitespace),
                                        lexing::ascribe,
                                        opt(lexing::whitespace),
                                    )),
                                    parse_type,
                                )),
                                |inner| inner.unwrap_or(ValueType::Inferred),
                            ),
                        ),
                    )),
                    lexing::close,
                ),
                opt(delimited(
                    lexing::whitespace,
                    preceded(pair(lexing::arrow, opt(lexing::whitespace)), parse_type),
                    lexing::whitespace,
                )),
                preceded(opt(lexing::whitespace), parse_sexpr_value),
            )),
            lexing::close,
        )),
        |(span, (_, _, arguments, return_type, eval))| SexprValue::Fn {
            arguments: arguments,
            return_type: return_type.unwrap_or(ValueType::Inferred),
            eval: Box::new(eval),
            span: Span { data: span },
        },
    )(input)
}

pub fn parse_type(input: &str) -> IResult<&str, ValueType> {
    alt((
        map(preceded(tag("?"), parse_type), |inner| {
            ValueType::Option(Box::new(inner))
        }),
        value(ValueType::Integer, tag("i32")),
        value(ValueType::Unit, tag("()")),
        value(ValueType::Zone, tag("zone")),
        value(ValueType::CardId, tag("card_id")),
        value(ValueType::Bool, tag("bool")),
        map(
            delimited(lexing::open_array, parse_type, lexing::close_array),
            |inner| ValueType::Array(Box::new(inner)),
        ), // value(ValueType::Fn(Box<FnTypeInfo>), tag("")),
        map(
            pair(
                delimited(lexing::open, many0(parse_type), lexing::close),
                preceded(lexing::arrow, parse_type),
            ),
            |(argument_types, return_type)| {
                ValueType::Fn(Box::new(FnTypeInfo {
                    argument_types,
                    return_type,
                }))
            },
        ),
    ))(input)
}

pub fn parse_if(input: &str) -> IResult<&str, SexprValue> {
    map(
        consumed(delimited(
            lexing::open,
            tuple((
                tag("if"),
                preceded(lexing::whitespace, parse_sexpr_value),
                preceded(lexing::whitespace, parse_sexpr_value),
                opt(preceded(lexing::whitespace, parse_sexpr_value)),
                opt(lexing::whitespace),
            )),
            lexing::close,
        )),
        |(span, (_, condition, if_true, if_false, _))| SexprValue::If {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false.unwrap_or(SexprValue::Unit(()))),
            span: Span { data: span },
        },
    )(input)
}

pub fn parse_sexpr_value(input: &str) -> IResult<&str, SexprValue> {
    alt((
        parse_if,
        parse_let,
        parse_fn,
        parse_none,
        parse_zone,
        parse_number_literal,
        parse_bool,
        parse_unit,
        parse_array,
        // parse_symbol must be second to last
        parse_symbol,
        map(parse_sexpr, SexprValue::Sexpr),
    ))(input)
}

pub fn parse_symbol(input: &str) -> IResult<&str, SexprValue> {
    map(lexing::identifier, |value| SexprValue::Symbol(value))(input)
}
pub fn parse_zone(input: &str) -> IResult<&str, SexprValue> {
    map_opt(lexing::identifier, |value| {
        Some(SexprValue::Zone(value.parse().ok()?))
    })(input)
}

pub fn parse_none(input: &str) -> IResult<&str, SexprValue> {
    value(SexprValue::None, tag("none"))(input)
}
pub fn parse_bool(input: &str) -> IResult<&str, SexprValue> {
    alt((
        value(SexprValue::Bool(true), tag("true")),
        value(SexprValue::Bool(false), tag("false")),
    ))(input)
}

pub fn parse_number_literal(input: &str) -> IResult<&str, SexprValue> {
    map(lexing::number, SexprValue::Integer)(input)
}

pub fn parse_unit(input: &str) -> IResult<&str, SexprValue> {
    value(SexprValue::Unit(()), tuple((lexing::open, lexing::close)))(input)
}

// pub fn parse_ascription(input: &str) -> IResult<&str, SexprValue> {
//     map(
//         tuple((
//             lexing::identifier,
//             delimited(
//                 opt(lexing::whitespace),
//                 lexing::ascribe,
//                 opt(lexing::whitespace),
//             ),
//             lexing::identifier,
//         )),
//         |(name, _, ty)| SexprValue::Ascription {
//             name: name.to_string(),
//             ty: ty.to_string(),
//         },
//     )(input)
// }

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SexprValue<'a> {
    Sexpr(Sexpr<'a>),
    Symbol(&'a str),
    Integer(i32),
    Bool(bool),
    Zone(ZoneId),
    Unit(()),
    None,
    Array {
        values: Vec<SexprValue<'a>>,
        span: Span<'a>,
    },
    Fn {
        arguments: Vec<(&'a str, ValueType)>,
        return_type: ValueType,
        eval: Box<SexprValue<'a>>,
        span: Span<'a>,
    },
    If {
        condition: Box<SexprValue<'a>>,
        if_true: Box<SexprValue<'a>>,
        if_false: Box<SexprValue<'a>>,
        span: Span<'a>,
    },
    Let {
        bindings: Vec<(&'a str, SexprValue<'a>)>,
        expr: Box<SexprValue<'a>>,
        span: Span<'a>,
    },
}

impl<'a> SexprValue<'a> {
    pub fn try_into_sexpr(self) -> Result<Sexpr<'a>, Self> {
        if let Self::Sexpr(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_symbol(self) -> Result<&'a str, Self> {
        if let Self::Symbol(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

impl<'a> Display for SexprValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SexprValue::Sexpr(Sexpr {
                target, arguments, ..
            }) => {
                write!(
                    f,
                    "({} {})",
                    target,
                    arguments
                        .iter()
                        .fold(String::new(), |acc, inner| format!("{} {}", acc, inner))
                )
            }
            SexprValue::Symbol(inner) => write!(f, "{}", inner),
            SexprValue::Integer(value) => write!(f, "{}", value),
            SexprValue::Zone(value) => write!(f, "{}", value),
            SexprValue::Unit(_) => write!(f, "()",),
            SexprValue::Fn {
                arguments,
                return_type,
                eval,
                ..
            } => write!(
                f,
                "fn({}) -> {} {{ {} }}",
                arguments
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(","),
                return_type,
                eval
            ),
            SexprValue::If {
                condition,
                if_true,
                if_false,
                ..
            } => write!(f, "(if {} {} {})", condition, if_true, if_false),
            SexprValue::Bool(value) => write!(f, "{}", value),
            SexprValue::Let { bindings, expr, .. } => write!(
                f,
                "(let ({}) {})",
                bindings
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(","),
                expr
            ),
            SexprValue::Array { values, .. } => {
                write!(
                    f,
                    "[{}]",
                    values
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            SexprValue::None => write!(f, "none"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Sexpr<'a> {
    pub target: &'a str,
    pub arguments: Vec<SexprValue<'a>>,
    pub span: Span<'a>,
}

#[cfg(test)]
mod test {
    use crate::parsing::{parse_sexpr, Sexpr, SexprValue};

    #[test]
    fn parse_sexpr_test() {
        assert_eq!(
            parse_sexpr("(my-symbol x)"),
            Ok((
                "",
                Sexpr {
                    target: "my-symbol",
                    arguments: vec![SexprValue::Symbol("x"),],
                    span: super::Span {
                        data: "(my-symbol x)",
                    }
                }
            ))
        );

        assert_eq!(
            parse_sexpr("(my-symbol (my-symbol x y))"),
            Ok((
                "",
                Sexpr {
                    target: "my-symbol",
                    arguments: vec![SexprValue::Sexpr(Sexpr {
                        target: "my-symbol",
                        arguments: vec![SexprValue::Symbol("x"), SexprValue::Symbol("y")],
                        span: super::Span {
                            data: "(my-symbol x y)",
                        }
                    })],
                    span: super::Span {
                        data: "(my-symbol (my-symbol x y))",
                    }
                }
            ))
        );
    }
}
