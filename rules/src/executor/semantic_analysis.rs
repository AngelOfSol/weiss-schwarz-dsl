pub(crate) mod hm;
mod symbol_validity;

use std::collections::HashSet;

use maplit::hashset;

use crate::{
    executor::{
        error::{CompileError, SymbolError},
        semantic_analysis::symbol_validity::check_symbol_validity,
        Executor, RUST_FN_TYPE_SCHEMES,
    },
    parsing::{Sexpr, SexprValue},
};

pub type SymbolTable<'a> = HashSet<&'a str>;

trait VerifySexpr {
    fn valid_target<'a, 'b, I>(&'a self, targets: I) -> Result<(), SymbolError<'a>>
    where
        I: Iterator<Item = &'a str> + 'b;
}

impl<'sexpr> VerifySexpr for Sexpr<'sexpr> {
    fn valid_target<'a, 'b, I>(&'a self, mut targets: I) -> Result<(), SymbolError<'a>>
    where
        I: Iterator<Item = &'a str> + 'b,
    {
        if targets.any(|item| item == self.target) {
            Ok(())
        } else {
            Err(SymbolError::InvalidFn {
                name: self.target,
                span: self.span,
            })
        }
    }
}

pub fn semantic_analysis<'a>(
    ast: &'a SexprValue,
    _executor: &Executor,
) -> Result<(), CompileError<'a>> {
    let default_symbol_table = hashset!("print")
        .into_iter()
        .chain(RUST_FN_TYPE_SCHEMES.keys().copied())
        .collect();

    check_symbol_validity(ast, default_symbol_table, _executor)?;
    hm::type_check(ast)?;
    Ok(())
}
