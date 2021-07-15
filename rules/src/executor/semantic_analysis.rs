pub(crate) mod hm;
mod symbol_validity;

use std::collections::HashSet;

use crate::{
    executor::{
        error::{CompileError, SymbolError},
        semantic_analysis::symbol_validity::check_symbol_validity,
        Executor, RUST_FN_TYPE_SCHEMES,
    },
    parsing::SexprValue,
};

pub type SymbolTable<'a> = HashSet<&'a str>;

trait VerifySexpr {
    fn valid_target<'a, 'b, I>(&'a self, targets: I) -> Result<(), SymbolError<'a>>
    where
        I: Iterator<Item = &'a str> + 'b;
}

impl<'sexpr> VerifySexpr for SexprValue<'sexpr> {
    fn valid_target<'a, 'b, I>(&'a self, mut targets: I) -> Result<(), SymbolError<'a>>
    where
        I: Iterator<Item = &'a str> + 'b,
    {
        if let SexprValue::Sexpr { target, span, .. } = self {
            if targets.any(|item| item == *target) {
                Ok(())
            } else {
                Err(SymbolError::InvalidFn {
                    name: target,
                    span: *span,
                })
            }
        } else {
            Ok(())
        }
    }
}

pub fn semantic_analysis<'a>(
    ast: &'a SexprValue,
    _executor: &Executor,
) -> Result<(), CompileError<'a>> {
    let default_symbol_table = RUST_FN_TYPE_SCHEMES.keys().copied().collect();

    check_symbol_validity(ast, default_symbol_table, _executor)?;
    hm::type_check(ast)?;
    Ok(())
}
