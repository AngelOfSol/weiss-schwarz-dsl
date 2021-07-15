mod arg_length;
pub(crate) mod hm;
mod symbol_validity;

use std::collections::HashSet;

use maplit::hashset;

use crate::{
    executor::{
        error::CompileError,
        semantic_analysis::{
            arg_length::check_arg_lengths, symbol_validity::check_symbol_validity,
        },
        Executor, RUST_FN_TYPE_SCHEMES,
    },
    parsing::{Sexpr, SexprValue},
};

pub type SymbolTable<'a> = HashSet<&'a str>;

trait VerifySexpr {
    fn valid_target<'a, 'b, I>(&'a self, targets: I) -> Result<(), CompileError<'a>>
    where
        I: Iterator<Item = &'a str> + 'b;
    fn arg_length(&self, len: usize) -> Result<Option<usize>, CompileError<'_>>;
}

impl<'sexpr> VerifySexpr for Sexpr<'sexpr> {
    fn arg_length(&self, len: usize) -> Result<Option<usize>, CompileError<'_>> {
        if self.arguments.len() == len {
            Ok(Some(len))
        } else {
            Err(CompileError::InvalidArgumentAmount {
                target: self.target,
                expected: len,
                found: self.arguments.len(),
            })
        }
    }

    fn valid_target<'a, 'b, I>(&'a self, mut targets: I) -> Result<(), CompileError<'a>>
    where
        I: Iterator<Item = &'a str> + 'b,
    {
        if targets.any(|item| item == self.target) {
            Ok(())
        } else {
            Err(CompileError::InvalidFn(self.target))
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

    let default_arg_length_table = RUST_FN_TYPE_SCHEMES
        .iter()
        .map(|(idx, ty_info)| (*idx, ty_info.ty.argument_count().unwrap()))
        .collect();

    check_symbol_validity(ast, default_symbol_table, _executor)?;
    check_arg_lengths(ast, default_arg_length_table, _executor)?;
    hm::type_check(ast)?;
    // type_check(ast, default_type_table, _executor)?;
    Ok(())
}
