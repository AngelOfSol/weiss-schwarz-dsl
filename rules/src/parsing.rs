pub mod lexing;

use std::{cell::RefCell, collections::HashMap, fmt::Display};

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{consumed, map, map_opt, opt, recognize, value},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;

use crate::executor::semantic_analysis::hm::{type_schemes::TypeScheme, types::Type, Fresh};
use crate::{executor::semantic_analysis::hm::types::TypeVariable, model::ZoneId};

pub type Span<'a> = LocatedSpan<&'a str>;

pub fn parse_sexpr(input: Span) -> IResult<Span, SexprValue> {
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
        |(span, (symbol, arguments, _))| SexprValue::Sexpr {
            target: symbol,
            arguments,
            span: span,
        },
    )(input)
}

pub fn parse_array(input: Span) -> IResult<Span, SexprValue> {
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
            SexprValue::Array { values: rest, span }
        },
    )(input)
}

pub fn parse_let(input: Span) -> IResult<Span, SexprValue> {
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
            span,
        },
    )(input)
}

pub fn parse_fn<'a>(input: Span<'a>) -> IResult<Span<'a>, SexprValue<'a>> {
    let fresh = RefCell::new(Fresh::default());
    let mapping = RefCell::new(HashMap::default());
    let (input, result) = map(
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
                                consumed(opt(preceded(
                                    tuple((
                                        opt(lexing::whitespace),
                                        lexing::ascribe,
                                        opt(lexing::whitespace),
                                    )),
                                    |input| parse_type_with_mapping(input, &fresh, &mapping),
                                ))),
                                |(span, inner)| {
                                    inner.unwrap_or(Type::type_var(fresh.borrow_mut().next(), span))
                                },
                            ),
                        ),
                    )),
                    lexing::close,
                ),
                consumed(opt(delimited(
                    lexing::whitespace,
                    preceded(pair(lexing::arrow, opt(lexing::whitespace)), |input| {
                        parse_type_with_mapping(input, &fresh, &mapping)
                    }),
                    lexing::whitespace,
                ))),
                preceded(opt(lexing::whitespace), parse_sexpr_value),
            )),
            lexing::close,
        )),
        |(span, (_, _, arguments, (ret_span, return_type), eval))| SexprValue::Fn {
            arguments: arguments,
            return_type: return_type.unwrap_or(Type::type_var(fresh.borrow_mut().next(), ret_span)),
            eval: Box::new(eval),
            span,
        },
    )(input)?;

    Ok((input, result))
}

pub fn parse_type(input: Span) -> IResult<Span, Type> {
    let fresh = RefCell::new(Fresh::default());
    let mapping = RefCell::new(HashMap::default());

    parse_type_with_mapping(input, &fresh, &mapping)
}

pub(crate) fn parse_type_scheme(input: Span) -> IResult<Span, TypeScheme> {
    let (input, ty) = parse_type(input)?;

    Ok((input, TypeScheme::generalize_all(ty)))
}

pub fn parse_type_with_mapping<'a>(
    input: Span<'a>,
    fresh: &RefCell<Fresh>,
    mapping: &RefCell<HashMap<&'a str, TypeVariable>>,
) -> IResult<Span<'a>, Type<'a>> {
    alt((
        map(recognize(tag("i32")), Type::integer),
        map(recognize(tag("()")), Type::unit),
        map(recognize(tag("zone")), Type::zone),
        map(recognize(tag("card")), Type::card),
        map(recognize(tag("bool")), Type::boolean),
        |input| parse_fn_type(input, fresh, mapping),
        map(
            preceded(
                tag("?"),
                consumed(|i| parse_type_with_mapping(i, fresh, mapping)),
            ),
            |(span, ty)| Type::option(ty, span),
        ),
        map(
            delimited(
                lexing::open_array,
                consumed(|i| parse_type_with_mapping(i, fresh, mapping)),
                lexing::close_array,
            ),
            |(span, ty)| Type::array(ty, span),
        ),
        map(consumed(lexing::identifier), |(span, ident)| {
            let mut mapping = mapping.borrow_mut();
            let mut fresh = fresh.borrow_mut();
            let tv = mapping.entry(ident).or_insert_with(|| fresh.next()).clone();
            Type::type_var(tv, span)
        }),
    ))(input)
}
pub fn parse_fn_type<'a>(
    input: Span<'a>,
    fresh: &RefCell<Fresh>,
    mapping: &RefCell<HashMap<&'a str, TypeVariable>>,
) -> IResult<Span<'a>, Type<'a>> {
    let (input, (span, (mut args, ret))) = consumed(tuple((
        delimited(
            tuple((tag("fn"), lexing::open)),
            separated_list0(
                tuple((opt(lexing::whitespace), tag(","), opt(lexing::whitespace))),
                |input| parse_type_with_mapping(input, fresh, mapping),
            ),
            lexing::close,
        ),
        opt(preceded(
            delimited(
                opt(lexing::whitespace),
                lexing::arrow,
                opt(lexing::whitespace),
            ),
            |input| parse_type_with_mapping(input, fresh, mapping),
        )),
    )))(input)?;
    args.push(ret.unwrap_or(Type::unit(input)));

    Ok((input, Type::function(args, span)))
}

pub fn parse_if(input: Span) -> IResult<Span, SexprValue> {
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
            if_false: Box::new(if_false.unwrap_or(SexprValue::Unit(span))),
            span,
        },
    )(input)
}

pub fn parse_sexpr_value(input: Span) -> IResult<Span, SexprValue> {
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
        parse_sexpr,
    ))(input)
}

pub fn parse_symbol(input: Span) -> IResult<Span, SexprValue> {
    map(consumed(lexing::identifier), |(span, value)| {
        SexprValue::Symbol(value, span)
    })(input)
}
pub fn parse_zone(input: Span) -> IResult<Span, SexprValue> {
    map_opt(consumed(lexing::identifier), |(span, value)| {
        Some(SexprValue::Zone(value.parse().ok()?, span))
    })(input)
}

pub fn parse_none(input: Span) -> IResult<Span, SexprValue> {
    map(tag("none"), |span| SexprValue::None(span))(input)
}
pub fn parse_bool(input: Span) -> IResult<Span, SexprValue> {
    map(
        alt((
            consumed(value(true, tag("true"))),
            consumed(value(false, tag("false"))),
        )),
        |(span, value)| SexprValue::Bool(value, span),
    )(input)
}

pub fn parse_number_literal(input: Span) -> IResult<Span, SexprValue> {
    map(consumed(lexing::number), |(span, value)| {
        SexprValue::Integer(value, span)
    })(input)
}

pub fn parse_unit(input: Span) -> IResult<Span, SexprValue> {
    map(
        consumed(tuple((lexing::open, lexing::close))),
        |(span, _)| SexprValue::Unit(span),
    )(input)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SexprValue<'a> {
    Sexpr {
        target: &'a str,
        arguments: Vec<SexprValue<'a>>,
        span: Span<'a>,
    },
    Symbol(&'a str, Span<'a>),
    Integer(i32, Span<'a>),
    Bool(bool, Span<'a>),
    Zone(ZoneId, Span<'a>),
    Unit(Span<'a>),
    None(Span<'a>),
    Array {
        values: Vec<SexprValue<'a>>,
        span: Span<'a>,
    },
    Fn {
        arguments: Vec<(&'a str, Type<'a>)>,
        return_type: Type<'a>,
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
    pub fn try_into_symbol(self) -> Result<&'a str, Self> {
        if let Self::Symbol(v, ..) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

impl<'a> Display for SexprValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SexprValue::Sexpr {
                target, arguments, ..
            } => {
                write!(
                    f,
                    "({} {})",
                    target,
                    arguments
                        .iter()
                        .fold(String::new(), |acc, inner| format!("{} {}", acc, inner))
                )
            }
            SexprValue::Symbol(inner, ..) => write!(f, "{}", inner),
            SexprValue::Integer(value, ..) => write!(f, "{}", value),
            SexprValue::Zone(value, ..) => write!(f, "{}", value),
            SexprValue::Unit(_) => write!(f, "()",),
            SexprValue::Bool(value, ..) => write!(f, "{}", value),
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
            SexprValue::None(..) => write!(f, "none"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternDeclaration<'a> {
    pub name: &'a str,
    pub(crate) type_scheme: TypeScheme<'a>,
    pub span: Span<'a>,
}

pub fn parse_program(input: Span) -> IResult<Span, (Vec<ExternDeclaration>, SexprValue)> {
    tuple((
        terminated(
            separated_list0(lexing::whitespace, parse_extern),
            opt(lexing::whitespace),
        ),
        parse_sexpr_value,
    ))(input)
}

pub fn parse_extern(input: Span) -> IResult<Span, ExternDeclaration> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(
                pair(tag("extern"), lexing::whitespace),
                pair(
                    lexing::identifier,
                    preceded(lexing::whitespace, parse_type_scheme),
                ),
            ),
            lexing::close,
        )),
        |(span, (name, type_scheme))| ExternDeclaration {
            name,
            type_scheme,
            span,
        },
    )(input)
}
