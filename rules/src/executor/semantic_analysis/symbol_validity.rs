use crate::{
    executor::{
        error::SymbolError,
        semantic_analysis::{SymbolTable, VerifySexpr},
    },
    parsing::SexprValue,
};

pub fn check_symbol_validity<'a>(
    ast: &'a SexprValue,
    mut symbols: SymbolTable<'a>,
) -> Result<(), SymbolError<'a>> {
    match ast {
        s @ SexprValue::Sexpr {
            target, arguments, ..
        } => match *target {
            "print" => check_symbol_validity(&arguments[0], symbols),
            _ => {
                for argument in arguments.iter() {
                    check_symbol_validity(argument, symbols.clone())?;
                }

                s.valid_target(symbols.iter().copied())
            }
        },
        SexprValue::Array { values, .. } => {
            for argument in values.iter() {
                check_symbol_validity(argument, symbols.clone())?;
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
        } => check_symbol_validity(condition, symbols.clone())
            .or_else(|_| check_symbol_validity(if_true, symbols.clone()))
            .or_else(|_| check_symbol_validity(if_false, symbols)),
        SexprValue::Fn {
            arguments, eval, ..
        } => {
            for (binding, _) in arguments {
                symbols.insert(*binding);
            }
            check_symbol_validity(eval, symbols)
        }
        SexprValue::Let { bindings, expr, .. } => {
            for (_, binding_expr) in bindings.iter() {
                check_symbol_validity(binding_expr, symbols.clone())?;
            }
            symbols.extend(bindings.iter().map(|(binding, _)| binding));

            check_symbol_validity(expr, symbols)
        }
        SexprValue::Integer(..)
        | SexprValue::Zone(..)
        | SexprValue::Unit(..)
        | SexprValue::Bool(..)
        | SexprValue::None(..) => Ok(()),
    }
}
