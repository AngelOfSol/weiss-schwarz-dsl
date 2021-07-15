pub(crate) mod hm;
mod symbol_validity;

use std::collections::HashSet;

use crate::{
    executor::{
        error::{CompileError, SymbolError},
        semantic_analysis::symbol_validity::check_symbol_validity,
        RUST_FN,
    },
    parsing::{ExternDeclaration, SexprValue},
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
    externs: &Vec<ExternDeclaration<'a>>,
) -> Result<(), CompileError<'a>> {
    let symbol_table = externs.iter().map(|item| item.name).collect();

    for decl in externs.iter().filter(|decl| decl.name != "print") {
        if !RUST_FN.contains_key(decl.name) {
            return Err(CompileError::InvalidExtern {
                name: decl.name,
                span: decl.span,
            });
        }
    }

    check_symbol_validity(ast, symbol_table)?;
    hm::type_check(ast, externs)?;
    Ok(())
}
