mod arg_length;
mod hm;
mod symbol_validity;
mod type_check;

use std::collections::{HashMap, HashSet};

use maplit::hashset;
use type_check::type_check;

use crate::{
    executor::{
        error::CompileError,
        semantic_analysis::{
            arg_length::check_arg_lengths, symbol_validity::check_symbol_validity,
        },
        value::ValueType,
        Executor, FnTypeInfo, RUST_FN_TYPES,
    },
    parsing::{Sexpr, SexprValue},
};

pub type SymbolTable<'a> = HashSet<&'a str>;

#[derive(Clone)]
pub struct TypeCheckTable<'a> {
    pub fn_types: HashMap<&'a str, FnTypeInfo>,
    pub binding_types: HashMap<&'a str, ValueType>,
}

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
        .chain(RUST_FN_TYPES.keys().copied())
        .collect();

    let default_arg_length_table = RUST_FN_TYPES
        .iter()
        .map(|(idx, ty_info)| (*idx, ty_info.argument_types.len()))
        .collect();

    let default_type_table = TypeCheckTable {
        fn_types: RUST_FN_TYPES.clone(),
        binding_types: HashMap::new(),
    };

    check_symbol_validity(ast, default_symbol_table, _executor)?;
    check_arg_lengths(ast, default_arg_length_table, _executor)?;
    type_check(ast, default_type_table, _executor)?;
    Ok(())
}
