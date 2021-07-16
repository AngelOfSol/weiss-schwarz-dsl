pub(crate) mod hm;
mod symbol_validity;

use std::collections::HashSet;

use crate::{
    executor::{
        error::{CompileError, SymbolError},
        semantic_analysis::{
            hm::{
                build_type_tree, infer, substitution::Substitution,
                type_environment::TypeEnvironment, type_schemes::TypeScheme, Fresh,
            },
            symbol_validity::check_symbol_validity,
        },
        RUST_FN,
    },
    parsing::{parse_type_scheme, ExternDeclaration, FunctionDefinition, SexprValue, Span},
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
    definitions: &'a Vec<FunctionDefinition<'a>>,
) -> Result<(), CompileError<'a>> {
    let symbol_table = externs
        .iter()
        .map(|item| item.name)
        .chain(definitions.iter().map(|item| item.name))
        .collect::<SymbolTable>();

    for decl in externs.iter().filter(|decl| decl.name != "print") {
        if !RUST_FN.contains_key(decl.name) {
            return Err(CompileError::InvalidExtern {
                name: decl.name,
                span: decl.span,
            });
        }
    }

    let mut fresh = Fresh::default();
    let mut env = TypeEnvironment::default();

    for decl in externs {
        env.map.insert(decl.name, decl.type_scheme.clone());
    }

    env.map.insert(
        "if",
        parse_type_scheme(Span::new("fn(bool, T, T) -> T"))
            .unwrap()
            .1,
    );

    let mut program_sub = Substitution::default();

    for def in definitions {
        check_symbol_validity(&def.eval, symbol_table.clone())?;
        env.map
            .insert(def.name, TypeScheme::generalize_all(def.ty.clone()));
    }
    check_symbol_validity(ast, symbol_table)?;

    for def in definitions {
        env.map
            .insert(def.name, TypeScheme::generalize_all(def.ty.clone()));
    }

    for def in definitions {
        let data = build_type_tree(&def.eval, &mut fresh);
        let (sub, _ty) = infer(&env, &mut fresh, &data)?;

        env.map.insert(def.name, TypeScheme::generalize_all(_ty));

        program_sub = program_sub.union(sub);
        env.apply(&program_sub);
    }

    let data = build_type_tree(ast, &mut fresh);

    let (sub, ty) = infer(&env, &mut fresh, &data)?;

    program_sub = program_sub.union(sub);

    ty.apply(&program_sub);

    env.apply(&program_sub);

    // hm::type_check(ast, externs)?;
    Ok(())
}
