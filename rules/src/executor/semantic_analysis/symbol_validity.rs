use crate::{
    executor::{error::SymbolError, semantic_analysis::SymbolTable},
    parsing::Sexpr,
};

pub fn check_symbol_validity<'a>(
    ast: &'a Sexpr,
    mut symbols: SymbolTable<'a>,
) -> Result<(), SymbolError<'a>> {
    match ast {
        s @ Sexpr::Eval { arguments, .. } => {
            for argument in arguments.iter() {
                check_symbol_validity(argument, symbols.clone())?;
            }
            Ok(())
        }
        Sexpr::Array { values, .. } => {
            for argument in values.iter() {
                check_symbol_validity(argument, symbols.clone())?;
            }
            Ok(())
        }
        Sexpr::Symbol(symbol, span) => {
            if symbols.contains(symbol) {
                Ok(())
            } else {
                Err(SymbolError::InvalidSymbol {
                    name: symbol,
                    span: *span,
                })
            }
        }
        Sexpr::If {
            condition,
            if_true,
            if_false,
            ..
        } => check_symbol_validity(condition, symbols.clone())
            .or_else(|_| check_symbol_validity(if_true, symbols.clone()))
            .or_else(|_| check_symbol_validity(if_false, symbols)),
        Sexpr::Fn {
            arguments, eval, ..
        } => {
            for (binding, _) in arguments {
                symbols.insert(*binding);
            }
            check_symbol_validity(eval, symbols)
        }
        Sexpr::Let { bindings, expr, .. } => {
            for (_, binding_expr) in bindings.iter() {
                check_symbol_validity(binding_expr, symbols.clone())?;
            }
            symbols.extend(bindings.iter().map(|(binding, _)| binding));

            check_symbol_validity(expr, symbols)
        }
        Sexpr::Seq {
            sub_expressions, ..
        } => {
            for expr in sub_expressions {
                check_symbol_validity(expr, symbols.clone())?;
            }
            Ok(())
        }
        Sexpr::Integer(..)
        | Sexpr::Zone(..)
        | Sexpr::Unit(..)
        | Sexpr::Bool(..)
        | Sexpr::None(..) => Ok(()),
    }
}
