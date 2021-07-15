use crate::{
    executor::{
        error::{CompileError, SymbolError},
        semantic_analysis::{SymbolTable, VerifySexpr},
        Executor,
    },
    parsing::SexprValue,
};

pub fn check_symbol_validity<'a>(
    ast: &'a SexprValue,
    mut symbols: SymbolTable<'a>,
    _executor: &Executor,
) -> Result<(), SymbolError<'a>> {
    match ast {
        SexprValue::Sexpr(sexpr) => match sexpr.target {
            "print" => check_symbol_validity(&sexpr.arguments[0], symbols, _executor),
            _ => {
                for argument in sexpr.arguments.iter() {
                    check_symbol_validity(argument, symbols.clone(), _executor)?;
                }

                sexpr.valid_target(symbols.iter().copied())
            }
        },
        SexprValue::Array { values, .. } => {
            for argument in values.iter() {
                check_symbol_validity(argument, symbols.clone(), _executor)?;
            }
            Ok(())
        }
        SexprValue::Symbol(symbol, span) => {
            if symbols.contains(symbol) {
                Ok(())
            } else {
                Err(SymbolError::InvalidSymbol {
                    name: symbol,
                    span: *span,
                })
            }
        }
        SexprValue::If {
            condition,
            if_true,
            if_false,
            ..
        } => check_symbol_validity(condition, symbols.clone(), _executor)
            .or_else(|_| check_symbol_validity(if_true, symbols.clone(), _executor))
            .or_else(|_| check_symbol_validity(if_false, symbols, _executor)),
        SexprValue::Fn {
            arguments, eval, ..
        } => {
            for (binding, _) in arguments {
                symbols.insert(*binding);
            }
            check_symbol_validity(eval, symbols, _executor)
        }
        SexprValue::Let { bindings, expr, .. } => {
            for (_, binding_expr) in bindings.iter() {
                check_symbol_validity(binding_expr, symbols.clone(), _executor)?;
            }
            symbols.extend(bindings.iter().map(|(binding, _)| binding));

            check_symbol_validity(expr, symbols, _executor)
        }
        SexprValue::Integer(..)
        | SexprValue::Zone(..)
        | SexprValue::Unit(..)
        | SexprValue::Bool(..)
        | SexprValue::None(..) => Ok(()),
    }
}
