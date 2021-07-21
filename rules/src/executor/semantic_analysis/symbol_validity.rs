use crate::{
    executor::{error::SymbolError, semantic_analysis::SymbolTable},
    parsing::Sexpr,
};

pub fn check_symbol_validity(ast: &Sexpr, mut symbols: SymbolTable) -> Result<(), SymbolError> {
    match ast {
        Sexpr::Eval { arguments, .. } => {
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
                    name: symbol.clone(),
                    span: span.clone(),
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
                symbols.insert(binding.clone());
            }
            check_symbol_validity(eval, symbols)
        }
        Sexpr::Let { bindings, expr, .. } => {
            for (_, binding_expr) in bindings.iter() {
                check_symbol_validity(binding_expr, symbols.clone())?;
            }
            symbols.extend(bindings.iter().map(|(binding, _)| binding).cloned());

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
