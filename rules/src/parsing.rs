pub mod lexing;

use std::{cell::RefCell, collections::HashMap, fmt::Display, iter::once};

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{consumed, map, map_opt, opt, recognize, value},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use nom_locate::LocatedSpan;

use crate::{executor::semantic_analysis::hm::types::TypeVariable, model::ZoneId};
use crate::{
    executor::semantic_analysis::hm::{type_schemes::TypeScheme, types::Type, Fresh},
    parsing::lexing::ws,
};

pub type SpanFileName<'a> = &'a str;

pub type Span<'a> = LocatedSpan<&'a str, SpanFileName<'a>>;

pub fn parse_sexpr(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open,
            tuple((lexing::identifier, many0(parse_sexpr_value))),
            lexing::close,
        )),
        |(span, (symbol, arguments))| Sexpr::Eval {
            target: symbol,
            arguments,
            span: span,
        },
    )(input)
}
pub fn parse_seq(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(ws(tag("seq")), many0(parse_sexpr_value)),
            lexing::close,
        )),
        |(span, sub_expressions)| Sexpr::Seq {
            sub_expressions,
            span: span,
        },
    )(input)
}

pub fn parse_array(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open_array,
            many1(parse_sexpr_value),
            lexing::close_array,
        )),
        |(span, rest)| Sexpr::Array { values: rest, span },
    )(input)
}

pub fn parse_let(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(
                ws(tag("let")),
                tuple((
                    delimited(
                        lexing::open,
                        many1(delimited(
                            lexing::open,
                            tuple((lexing::identifier, parse_sexpr_value)),
                            lexing::close,
                        )),
                        lexing::close,
                    ),
                    parse_sexpr_value,
                )),
            ),
            lexing::close,
        )),
        |(span, (bindings, expr))| Sexpr::Let {
            bindings,
            expr: Box::new(expr),
            span,
        },
    )(input)
}

pub fn parse_fn<'a>(input: Span<'a>) -> IResult<Span<'a>, Sexpr<'a>> {
    let fresh = RefCell::new(Fresh::default());
    let mapping = RefCell::new(HashMap::default());
    let (input, result) = map(
        consumed(delimited(
            lexing::open,
            preceded(
                ws(tag("fn")),
                tuple((
                    delimited(
                        lexing::open,
                        many0(pair(
                            lexing::identifier,
                            map(
                                consumed(opt(preceded(lexing::ascribe, |input| {
                                    parse_type_with_mapping(input, &fresh, &mapping)
                                }))),
                                |(span, inner)| {
                                    inner.unwrap_or(Type::type_var(fresh.borrow_mut().next(), span))
                                },
                            ),
                        )),
                        lexing::close,
                    ),
                    consumed(opt(preceded(lexing::arrow, |input| {
                        parse_type_with_mapping(input, &fresh, &mapping)
                    }))),
                    parse_sexpr_value,
                )),
            ),
            lexing::close,
        )),
        |(span, (arguments, (ret_span, return_type), eval))| Sexpr::Fn {
            arguments: arguments,
            return_type: return_type.unwrap_or(Type::type_var(fresh.borrow_mut().next(), ret_span)),
            eval: Box::new(eval),
            span,
        },
    )(input)?;

    Ok((input, result))
}

pub fn parse_type<'a>(input: Span<'a>) -> IResult<Span<'a>, Type<'a>> {
    let fresh = RefCell::new(Fresh::default());
    let mapping = RefCell::new(HashMap::default());

    parse_type_with_mapping(input, &fresh, &mapping)
}

pub(crate) fn parse_type_scheme<'a>(input: Span<'a>) -> IResult<Span<'a>, TypeScheme<'a>> {
    let (input, ty) = ws(parse_type)(input)?;

    Ok((input, TypeScheme::generalize_all(ty)))
}

pub fn parse_type_with_mapping<'a>(
    input: Span<'a>,
    fresh: &RefCell<Fresh>,
    mapping: &RefCell<HashMap<&'a str, TypeVariable>>,
) -> IResult<Span<'a>, Type<'a>> {
    ws(alt((
        map(recognize(ws(tag("i32"))), Type::integer),
        map(recognize(ws(tag("()"))), Type::unit),
        map(recognize(ws(tag("zone"))), Type::zone),
        map(recognize(ws(tag("card"))), Type::card),
        map(recognize(ws(tag("bool"))), Type::boolean),
        |input| parse_fn_type(input, fresh, mapping),
        map(
            preceded(
                ws(tag("?")),
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
    )))(input)
}
pub fn parse_fn_type<'a>(
    input: Span<'a>,
    fresh: &RefCell<Fresh>,
    mapping: &RefCell<HashMap<&'a str, TypeVariable>>,
) -> IResult<Span<'a>, Type<'a>> {
    let (input, (span, (mut args, ret))) = consumed(tuple((
        delimited(
            tuple((tag("fn"), lexing::open)),
            separated_list0(ws(tag(",")), |input| {
                parse_type_with_mapping(input, fresh, mapping)
            }),
            lexing::close,
        ),
        opt(preceded(lexing::arrow, |input| {
            parse_type_with_mapping(input, fresh, mapping)
        })),
    )))(input)?;
    args.push(ret.unwrap_or(Type::unit(input)));

    Ok((input, Type::function(args, span)))
}

pub fn parse_if(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(
                tag("if"),
                tuple((parse_sexpr_value, parse_sexpr_value, opt(parse_sexpr_value))),
            ),
            lexing::close,
        )),
        |(span, (condition, if_true, if_false))| Sexpr::If {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false.unwrap_or(Sexpr::Unit(span))),
            span,
        },
    )(input)
}

pub fn parse_sexpr_value(input: Span) -> IResult<Span, Sexpr> {
    ws(alt((
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
        parse_seq,
        parse_sexpr,
    )))(input)
}

pub fn parse_symbol(input: Span) -> IResult<Span, Sexpr> {
    map(consumed(lexing::identifier), |(span, value)| {
        Sexpr::Symbol(value, span)
    })(input)
}
pub fn parse_zone(input: Span) -> IResult<Span, Sexpr> {
    map_opt(consumed(lexing::identifier), |(span, value)| {
        Some(Sexpr::Zone(value.parse().ok()?, span))
    })(input)
}

pub fn parse_none(input: Span) -> IResult<Span, Sexpr> {
    map(tag("none"), |span| Sexpr::None(span))(input)
}
pub fn parse_bool(input: Span) -> IResult<Span, Sexpr> {
    map(
        alt((
            consumed(value(true, tag("true"))),
            consumed(value(false, tag("false"))),
        )),
        |(span, value)| Sexpr::Bool(value, span),
    )(input)
}

pub fn parse_number_literal(input: Span) -> IResult<Span, Sexpr> {
    map(consumed(lexing::number), |(span, value)| {
        Sexpr::Integer(value, span)
    })(input)
}

pub fn parse_unit(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(tuple((lexing::open, lexing::close))),
        |(span, _)| Sexpr::Unit(span),
    )(input)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Sexpr<'a> {
    Eval {
        target: &'a str,
        arguments: Vec<Sexpr<'a>>,
        span: Span<'a>,
    },
    Symbol(&'a str, Span<'a>),
    Integer(i32, Span<'a>),
    Bool(bool, Span<'a>),
    Zone(ZoneId, Span<'a>),
    Unit(Span<'a>),
    None(Span<'a>),
    Array {
        values: Vec<Sexpr<'a>>,
        span: Span<'a>,
    },
    Fn {
        arguments: Vec<(&'a str, Type<'a>)>,
        return_type: Type<'a>,
        eval: Box<Sexpr<'a>>,
        span: Span<'a>,
    },
    If {
        condition: Box<Sexpr<'a>>,
        if_true: Box<Sexpr<'a>>,
        if_false: Box<Sexpr<'a>>,
        span: Span<'a>,
    },
    Let {
        bindings: Vec<(&'a str, Sexpr<'a>)>,
        expr: Box<Sexpr<'a>>,
        span: Span<'a>,
    },
    Seq {
        sub_expressions: Vec<Sexpr<'a>>,
        span: Span<'a>,
    },
}

impl<'a> Sexpr<'a> {
    pub fn try_into_symbol(self) -> Result<&'a str, Self> {
        if let Self::Symbol(v, ..) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

impl<'a> Display for Sexpr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sexpr::Eval {
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
            Sexpr::Symbol(inner, ..) => write!(f, "{}", inner),
            Sexpr::Integer(value, ..) => write!(f, "{}", value),
            Sexpr::Zone(value, ..) => write!(f, "{}", value),
            Sexpr::Unit(_) => write!(f, "()",),
            Sexpr::Bool(value, ..) => write!(f, "{}", value),
            Sexpr::Fn {
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
            Sexpr::If {
                condition,
                if_true,
                if_false,
                ..
            } => write!(f, "(if {} {} {})", condition, if_true, if_false),
            Sexpr::Let { bindings, expr, .. } => write!(
                f,
                "(let ({}) {})",
                bindings
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(","),
                expr
            ),
            Sexpr::Array { values, .. } => {
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
            Sexpr::None(..) => write!(f, "none"),
            Sexpr::Seq {
                sub_expressions, ..
            } => write!(
                f,
                "(seq {})",
                sub_expressions
                    .iter()
                    .map(|expr| format!("{}", expr))
                    .collect::<Vec<_>>()
                    .join(" "),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternDeclaration<'a> {
    pub name: &'a str,
    pub(crate) type_scheme: TypeScheme<'a>,
    pub span: Span<'a>,
}

#[derive(Debug)]
enum Preamble<'a> {
    Extern(ExternDeclaration<'a>),
    Definition(FunctionDefinition<'a>),
}

impl<'a> Preamble<'a> {
    fn try_into_extern(self) -> Result<ExternDeclaration<'a>, Self> {
        if let Self::Extern(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    fn try_into_definition(self) -> Result<FunctionDefinition<'a>, Self> {
        if let Self::Definition(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

pub fn parse_program(
    input: Span,
) -> IResult<Span, (Vec<ExternDeclaration>, Vec<FunctionDefinition>, Sexpr)> {
    map(
        tuple((
            many0(alt((
                map(parse_extern, Preamble::Extern),
                map(parse_fn_definition, Preamble::Definition),
            ))),
            parse_sexpr_value,
        )),
        |(preambles, program)| {
            let (lhs, rhs) = preambles
                .into_iter()
                .partition::<Vec<_>, _>(|value| matches!(value, Preamble::Extern(..)));
            let externs: Vec<_> = lhs
                .into_iter()
                .map(|item| item.try_into_extern().ok().unwrap())
                .collect();
            let definitions: Vec<_> = rhs
                .into_iter()
                .map(|item| item.try_into_definition().ok().unwrap())
                .collect();

            (externs, definitions, program)
        },
    )(input)
}

pub fn parse_extern(input: Span) -> IResult<Span, ExternDeclaration> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(
                ws(tag("extern")),
                pair(lexing::identifier, parse_type_scheme),
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

#[derive(Debug, Clone)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub eval: Sexpr<'a>,
    pub span: Span<'a>,
    pub ty: Type<'a>,
}

pub fn parse_fn_definition(input: Span) -> IResult<Span, FunctionDefinition> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(ws(tag("define")), pair(lexing::identifier, ws(parse_fn))),
            lexing::close,
        )),
        |(span, (name, eval))| {
            let ty = if let Sexpr::Fn {
                arguments,
                return_type,
                span,
                ..
            } = &eval
            {
                Type::function(
                    arguments
                        .iter()
                        .map(|(_, ty)| ty.clone())
                        .chain(once(return_type.clone()))
                        .collect(),
                    *span,
                )
            } else {
                unreachable!()
            };

            FunctionDefinition {
                name,
                eval,
                span,
                ty,
            }
        },
    )(input)
}
