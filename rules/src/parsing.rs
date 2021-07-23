pub mod input_type;
pub mod lexing;

use std::{cell::RefCell, collections::HashMap, fmt::Display};

use arcstr::Substr;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{all_consuming, consumed, cut, map, map_opt, opt, recognize, value},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult as NomResult,
};
use nom_locate::LocatedSpan;

use crate::{
    executor::semantic_analysis::hm::types::TypeVariable, model::ZoneId, parsing::input_type::Input,
};
use crate::{
    executor::semantic_analysis::hm::{type_schemes::TypeScheme, types::Type, Fresh},
    parsing::lexing::ws,
};

pub use nom::{
    error::{VerboseError, VerboseErrorKind},
    Err,
};

pub type SpanContent = Input;

pub type ParseError = nom::error::VerboseError<Span>;

pub type Span = LocatedSpan<SpanContent, Substr>;

pub type IResult<I, O, E = ParseError> = NomResult<I, O, E>;

pub fn make_span<S1, S2>(data: S1, file_name: S2) -> Span
where
    Substr: From<S1> + From<S2>,
{
    Span::new_extra(Input(Substr::from(data)), Substr::from(file_name))
}

pub fn parse_sexpr(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open,
            cut(many1(parse_sexpr_value)),
            lexing::close,
        )),
        |(span, arguments)| Sexpr::Eval { arguments, span },
    )(input)
}
pub fn parse_seq(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(ws(tag("seq")), cut(many0(parse_sexpr_value))),
            lexing::close,
        )),
        |(span, sub_expressions)| Sexpr::Seq {
            sub_expressions,
            span,
        },
    )(input)
}

pub fn parse_array(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open_array,
            cut(many0(parse_sexpr_value)),
            cut(lexing::close_array),
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
                cut(tuple((
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
                ))),
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

pub fn parse_fn(input: Span) -> IResult<Span, Sexpr> {
    let fresh = RefCell::new(Fresh::default());
    let mapping = RefCell::new(HashMap::default());
    let (input, result) = map(
        consumed(delimited(
            lexing::open,
            preceded(
                ws(tag("fn")),
                cut(tuple((
                    delimited(
                        lexing::open,
                        many0(pair(
                            lexing::identifier,
                            map(
                                consumed(opt(preceded(lexing::ascribe, |input| {
                                    parse_type_with_mapping(input, &fresh, &mapping)
                                }))),
                                |(span, inner)| {
                                    inner.unwrap_or(Type::type_var(
                                        fresh.borrow_mut().next_type_variable(),
                                        span,
                                    ))
                                },
                            ),
                        )),
                        lexing::close,
                    ),
                    opt(preceded(lexing::arrow, |input| {
                        parse_type_with_mapping(input, &fresh, &mapping)
                    })),
                    parse_sexpr_value,
                ))),
            ),
            lexing::close,
        )),
        |(span, (arguments, return_type, eval))| Sexpr::Fn {
            arguments: arguments,
            return_type: return_type,
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
    let (input, ty) = ws(parse_type)(input)?;

    Ok((input, TypeScheme::generalize_all(ty)))
}

pub fn parse_type_with_mapping(
    input: Span,
    fresh: &RefCell<Fresh>,
    mapping: &RefCell<HashMap<Substr, TypeVariable>>,
) -> IResult<Span, Type> {
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
            let tv = mapping
                .entry(ident)
                .or_insert_with(|| fresh.next_type_variable())
                .clone();
            Type::type_var(tv, span)
        }),
    )))(input)
}
pub fn parse_fn_type<'a>(
    input: Span,
    fresh: &RefCell<Fresh>,
    mapping: &RefCell<HashMap<Substr, TypeVariable>>,
) -> IResult<Span, Type> {
    let (input, (span, (mut args, ret))) = consumed(tuple((
        delimited(
            tuple((tag("fn"), lexing::open)),
            cut(separated_list0(ws(tag(",")), |input| {
                parse_type_with_mapping(input, fresh, mapping)
            })),
            lexing::close,
        ),
        opt(preceded(lexing::arrow, |input| {
            parse_type_with_mapping(input, fresh, mapping)
        })),
    )))(input)?;
    args.push(ret.unwrap_or(Type::unit(input.clone())));

    Ok((input, Type::function(args, span)))
}

pub fn parse_if(input: Span) -> IResult<Span, Sexpr> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(
                tag("if"),
                cut(tuple((
                    parse_sexpr_value,
                    parse_sexpr_value,
                    opt(parse_sexpr_value),
                ))),
            ),
            lexing::close,
        )),
        |(span, (condition, if_true, if_false))| Sexpr::If {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false.unwrap_or(Sexpr::Unit(span.clone()))),
            span,
        },
    )(input)
}

pub fn parse_sexpr_value(input: Span) -> IResult<Span, Sexpr> {
    ws(alt((
        parse_if,
        parse_let,
        parse_fn,
        parse_seq,
        parse_none,
        parse_zone,
        parse_number_literal,
        parse_bool,
        parse_unit,
        parse_array,
        // parse_symbol must be second to last
        parse_symbol,
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
    map(recognize(tag("()")), Sexpr::Unit)(input)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Sexpr {
    Eval {
        arguments: Vec<Sexpr>,
        span: Span,
    },
    Symbol(Substr, Span),
    Integer(i32, Span),
    Bool(bool, Span),
    Zone(ZoneId, Span),
    Unit(Span),
    None(Span),
    Array {
        values: Vec<Sexpr>,
        span: Span,
    },
    Fn {
        arguments: Vec<(Substr, Type)>,
        return_type: Option<Type>,
        eval: Box<Sexpr>,
        span: Span,
    },
    If {
        condition: Box<Sexpr>,
        if_true: Box<Sexpr>,
        if_false: Box<Sexpr>,
        span: Span,
    },
    Let {
        bindings: Vec<(Substr, Sexpr)>,
        expr: Box<Sexpr>,
        span: Span,
    },
    Seq {
        sub_expressions: Vec<Sexpr>,
        span: Span,
    },
}

impl Sexpr {
    pub fn try_into_symbol(self) -> Result<Substr, Self> {
        if let Self::Symbol(v, ..) = self {
            Ok(v.clone())
        } else {
            Err(self)
        }
    }
}

impl<'a> Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sexpr::Eval { arguments, .. } => {
                write!(
                    f,
                    "({})",
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
                return_type
                    .as_ref()
                    .map(|x| x.to_string())
                    .unwrap_or("()".to_string()),
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
pub struct ExternDeclaration {
    pub name: Substr,
    pub type_scheme: TypeScheme,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Include {
    pub path: Substr,
}

#[derive(Debug)]
enum Preamble {
    Extern(ExternDeclaration),
    Definition(FunctionDefinition),
    Include(Include),
}

impl Preamble {
    fn try_into_extern(self) -> Result<ExternDeclaration, Self> {
        if let Self::Extern(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    fn try_into_definition(self) -> Result<FunctionDefinition, Self> {
        if let Self::Definition(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    fn try_into_include(self) -> Result<Include, Self> {
        if let Self::Include(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

pub struct IncludedProgram {
    pub externs: Vec<ExternDeclaration>,
    pub defines: Vec<FunctionDefinition>,
}

pub fn parse_included_file(input: Span) -> IResult<Span, IncludedProgram> {
    map(
        all_consuming(many0(alt((
            map(parse_extern, Preamble::Extern),
            map(parse_fn_definition, Preamble::Definition),
        )))),
        |preambles| {
            let (externs, preambles) = preambles
                .into_iter()
                .partition::<Vec<_>, _>(|value| matches!(value, Preamble::Extern(..)));
            let externs: Vec<_> = externs
                .into_iter()
                .map(|item| item.try_into_extern().ok().unwrap())
                .collect();
            let (defines, _) = preambles
                .into_iter()
                .partition::<Vec<_>, _>(|value| matches!(value, Preamble::Definition(..)));
            let defines: Vec<_> = defines
                .into_iter()
                .map(|item| item.try_into_definition().ok().unwrap())
                .collect();

            IncludedProgram { externs, defines }
        },
    )(input)
}

pub struct ParsedProgram {
    pub externs: Vec<ExternDeclaration>,
    pub defines: Vec<FunctionDefinition>,
    pub includes: Vec<Include>,
    pub expr: Sexpr,
}

pub fn parse_program(input: Span) -> IResult<Span, ParsedProgram> {
    map(
        all_consuming(tuple((
            many0(alt((
                map(parse_extern, Preamble::Extern),
                map(parse_fn_definition, Preamble::Definition),
                map(parse_include, Preamble::Include),
            ))),
            consumed(many1(parse_sexpr_value)),
        ))),
        |(preambles, (span, sub_expressions))| {
            let (externs, preambles) = preambles
                .into_iter()
                .partition::<Vec<_>, _>(|value| matches!(value, Preamble::Extern(..)));
            let externs: Vec<_> = externs
                .into_iter()
                .map(|item| item.try_into_extern().ok().unwrap())
                .collect();
            let (definitions, includes) = preambles
                .into_iter()
                .partition::<Vec<_>, _>(|value| matches!(value, Preamble::Definition(..)));
            let defines: Vec<_> = definitions
                .into_iter()
                .map(|item| item.try_into_definition().ok().unwrap())
                .collect();
            let includes: Vec<_> = includes
                .into_iter()
                .map(|item| item.try_into_include().ok().unwrap())
                .collect();

            ParsedProgram {
                externs,
                defines,
                includes,
                expr: Sexpr::Seq {
                    sub_expressions,
                    span,
                },
            }
        },
    )(input)
}

pub fn parse_extern(input: Span) -> IResult<Span, ExternDeclaration> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(
                ws(tag("extern")),
                cut(pair(lexing::identifier, parse_type_scheme)),
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
pub struct FunctionDefinition {
    pub name: Substr,
    pub eval: Sexpr,
    pub span: Span,
}

pub fn parse_fn_definition(input: Span) -> IResult<Span, FunctionDefinition> {
    map(
        consumed(delimited(
            lexing::open,
            preceded(
                ws(tag("define")),
                cut(pair(lexing::identifier, ws(parse_fn))),
            ),
            lexing::close,
        )),
        |(span, (name, eval))| FunctionDefinition { name, eval, span },
    )(input)
}

pub fn parse_include(input: Span) -> IResult<Span, Include> {
    map(
        delimited(
            lexing::open,
            preceded(ws(tag("include")), cut(lexing::raw_string)),
            lexing::close,
        ),
        |path| Include { path },
    )(input)
}
