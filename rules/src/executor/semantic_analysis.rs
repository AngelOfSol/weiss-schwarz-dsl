pub(crate) mod constraints;
pub(crate) mod hm;
mod symbol_validity;

use std::{collections::HashSet, iter::once};

use crate::{
    executor::{
        error::CompileError,
        semantic_analysis::{
            constraints::{infer, unify},
            hm::{
                type_environment::TypeEnvironment, type_tree::build_type_tree, types::Type, Fresh,
                TypedAst,
            },
            symbol_validity::check_symbol_validity,
        },
        RUST_FN,
    },
    parsing::{ExternDeclaration, FunctionDefinition, Sexpr},
};

pub type SymbolTable<'a> = HashSet<&'a str>;

pub fn semantic_analysis<'a>(
    ast: &'a Sexpr,
    externs: &Vec<ExternDeclaration<'a>>,
    definitions: &'a Vec<FunctionDefinition<'a>>,
) -> Result<TypedAst<'a>, CompileError<'a>> {
    let symbol_table = externs
        .iter()
        .map(|item| item.name)
        .chain(definitions.iter().map(|item| item.name))
        .collect::<SymbolTable>();

    let mut fresh = Fresh::default();
    let mut env = TypeEnvironment::default();

    for decl in externs {
        env.map.insert(decl.name, decl.type_scheme.clone());
    }

    let mut extern_errors = externs
        .iter()
        .filter_map(|decl| {
            if RUST_FN.contains_key(decl.name) || decl.name == "print" {
                None
            } else {
                Some(CompileError::InvalidExtern {
                    name: decl.name,
                    span: decl.span,
                })
            }
        })
        .peekable();

    let mut symbol_errors = definitions
        .iter()
        .map(|decl| &decl.eval)
        .chain(once(ast))
        .filter_map(|expr| check_symbol_validity(expr, symbol_table.clone()).err())
        .map(CompileError::from)
        .peekable();

    if symbol_errors.peek().is_some() {
        return Err(CompileError::List(
            symbol_errors.chain(extern_errors).collect(),
        ));
    }

    let data = build_type_tree(ast.clone(), &mut fresh);

    let upper = TypedAst::Let {
        bindings: definitions
            .iter()
            .map(|def| (def.name, build_type_tree(def.eval.clone(), &mut fresh)))
            .collect(),
        span: *data.span(),
        ty: Type::Var(fresh.next_type_variable(), *data.span()),
        expr: Box::new(data),
    };

    let mut unifiers = Default::default();
    infer(&mut unifiers, &env, &mut fresh, &upper);

    let (program_substitutions, unification_errors) = unify(unifiers);

    if !unification_errors.is_empty() || extern_errors.peek().is_some() {
        return Err(CompileError::List(
            unification_errors
                .into_iter()
                .map(CompileError::from)
                .chain(extern_errors)
                .collect(),
        ));
    }

    Ok({
        let mut upper = upper;
        println!("{}", upper);
        upper.apply(&program_substitutions);
        println!("{}", upper);
        upper
    })
}
