use std::collections::HashMap;

use crate::{
    executor::{
        error::CompileError, semantic_analysis::VerifySexpr, value::ValueType, Executor,
        RUST_FN_TYPES,
    },
    parsing::{Sexpr, SexprValue},
};

pub type ArgLengthTable<'a> = HashMap<&'a str, usize>;

pub fn check_arg_lengths<'a>(
    ast: &'a SexprValue,
    mut arg_lengths: ArgLengthTable<'a>,
    _executor: &Executor,
) -> Result<Option<usize>, CompileError<'a>> {
    match ast {
        SexprValue::Sexpr(
            sexpr
            @ Sexpr {
                target: "print",
                arguments,
            },
        ) => {
            for argument in arguments.iter() {
                check_arg_lengths(argument, arg_lengths.clone(), _executor)?;
            }
            sexpr.arg_length(1)
        }
        SexprValue::Sexpr(sexpr) => {
            for argument in sexpr.arguments.iter() {
                check_arg_lengths(argument, arg_lengths.clone(), _executor)?;
            }
            sexpr.arg_length(arg_lengths[sexpr.target])
        }
        SexprValue::Fn {
            arguments, eval, ..
        } => {
            for (binding, value) in arguments {
                match value {
                    ValueType::Fn(inner) => {
                        arg_lengths.insert(binding, inner.argument_types.len());
                    }
                    _ => (),
                }
            }
            check_arg_lengths(eval, arg_lengths, _executor)?;
            Ok(Some(arguments.len()))
        }
        SexprValue::Let { bindings, expr } => {
            for (binding, value) in bindings.iter() {
                if let Some(arg_count) = check_arg_lengths(value, arg_lengths.clone(), _executor)? {
                    arg_lengths.insert(*binding, arg_count);
                }
            }
            check_arg_lengths(&expr, arg_lengths, _executor)
        }
        SexprValue::If {
            condition,
            if_true,
            if_false,
        } => check_arg_lengths(condition, arg_lengths.clone(), _executor)
            .or_else(|_| check_arg_lengths(if_true, arg_lengths.clone(), _executor))
            .or_else(|_| check_arg_lengths(if_false, arg_lengths, _executor)),
        SexprValue::Symbol(_)
        | SexprValue::Integer(_)
        | SexprValue::Zone(_)
        | SexprValue::Bool(_)
        | SexprValue::Unit(_)
        | SexprValue::Array(_)
        | SexprValue::None => Ok(None),
    }
}
