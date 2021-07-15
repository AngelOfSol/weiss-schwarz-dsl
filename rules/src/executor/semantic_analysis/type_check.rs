use std::{collections::HashMap, iter::once};

use crate::{
    executor::value::ValueType,
    executor::{error::CompileError, semantic_analysis::TypeCheckTable, Executor, FnTypeInfo},
    parsing::{Sexpr, SexprValue},
};

pub fn type_check<'a>(
    ast: &'a SexprValue,
    types: TypeCheckTable<'a>,
    _executor: &Executor,
) -> Result<ValueType, CompileError<'a>> {
    match ast {
        SexprValue::Sexpr(Sexpr {
            target: "print",
            arguments,
            ..
        }) => type_check(&arguments[0], types, _executor),

        SexprValue::Sexpr(Sexpr {
            target: "some",
            arguments,
            ..
        }) => {
            let inner = type_check(&arguments[0], types, _executor)?;
            if inner == ValueType::Unit {
                Ok(ValueType::Unit)
            } else {
                Ok(ValueType::Option(Box::new(inner)))
            }
        }
        SexprValue::Sexpr(sexpr) => {
            let type_info = types
                .fn_types
                .get(sexpr.target)
                .or_else(|| {
                    types
                        .binding_types
                        .get(sexpr.target)
                        .and_then(|v| v.as_fn())
                })
                .ok_or_else(|| CompileError::InvalidFnType {
                    target: sexpr.target,
                    found: types.binding_types[sexpr.target].clone(),
                })?;
            for (expected, expr) in type_info
                .argument_types
                .iter()
                .cloned()
                .zip(sexpr.arguments.iter())
            {
                let found = type_check(expr, types.clone(), _executor)?;
                if !expected.matches(&found) {
                    return Err(CompileError::InvalidType {
                        target: sexpr.target,
                        expected,
                        found,
                        value: expr,
                    });
                }
            }
            Ok(type_info.return_type.clone())
        }
        SexprValue::Symbol(binding, ..) => Ok(types.binding_types[*binding].clone()),
        SexprValue::Integer(_) => Ok(ValueType::Integer),
        SexprValue::Zone(_) => Ok(ValueType::Zone),
        SexprValue::Unit(_) => Ok(ValueType::Unit),
        SexprValue::Bool(_) => Ok(ValueType::Bool),
        SexprValue::None => Ok(ValueType::Option(Box::new(ValueType::Unit))),
        array @ SexprValue::Array(values) => {
            let types = values
                .iter()
                .map(|inner| type_check(inner, types.clone(), _executor))
                .collect::<Result<Vec<_>, _>>()?;

            let ret = types.first().unwrap().clone();

            for ty in types {
                if !ty.matches(&ret) {
                    return Err(CompileError::InvalidType {
                        target: "array value",
                        expected: ty,
                        found: ret,
                        value: array,
                    });
                }
            }

            Ok(ValueType::Array(Box::new(ret)))
        }
        SexprValue::Fn {
            arguments,
            return_type,
            eval,
            ..
        } => {
            let mut types = types;
            for (binding, ty) in arguments.iter().cloned() {
                types.binding_types.insert(binding, ty);
            }
            let ty = type_check(eval, types, _executor)?;
            if !ty.matches(return_type) {
                Err(CompileError::InvalidType {
                    target: "fn return type",
                    expected: return_type.clone(),
                    found: ty,
                    value: eval,
                })
            } else {
                Ok(ValueType::Fn(Box::new(FnTypeInfo {
                    argument_types: arguments.iter().map(|(_, ty)| ty).cloned().collect(),
                    return_type: return_type.clone(),
                })))
            }
        }
        SexprValue::Let { bindings, expr, .. } => {
            let mut types = types;

            for (key, value) in bindings.iter() {
                types
                    .binding_types
                    .insert(*key, type_check(value, types.clone(), _executor)?);
            }
            //

            type_check(expr, types, _executor)
        }
        SexprValue::If {
            condition,
            if_true,
            if_false,
            ..
        } => {
            let condition_type = type_check(condition, types.clone(), _executor)?;
            if condition_type != ValueType::Bool {
                return Err(CompileError::InvalidCondition {
                    found: condition_type,
                    value: condition,
                });
            }

            let true_type = type_check(if_true, types.clone(), _executor)?;
            let false_type = type_check(if_false, types, _executor)?;
            if !true_type.matches(&false_type) {
                Err(CompileError::IncompatibleTypes {
                    true_type,
                    false_type,
                    value: if_false,
                })
            } else {
                Ok(true_type)
            }
        }
    }
}
