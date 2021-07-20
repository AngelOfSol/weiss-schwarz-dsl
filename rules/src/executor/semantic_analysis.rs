pub(crate) mod constraints;
pub(crate) mod hm;
mod symbol_validity;

use std::collections::HashSet;

use crate::{
    executor::{
        error::{CompileError, TypeError},
        semantic_analysis::{
            constraints::{infer2, unify2},
            hm::{
                infer, type_environment::TypeEnvironment, type_tree::build_type_tree, types::Type,
                Fresh, TypedAst,
            },
            symbol_validity::check_symbol_validity,
        },
        RUST_FN,
    },
    parsing::{parse_type_scheme, ExternDeclaration, FunctionDefinition, Sexpr, Span},
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
        parse_type_scheme(Span::new_extra("fn(bool, T, T) -> T", "<internal>"))
            .unwrap()
            .1,
    );

    for def in definitions {
        check_symbol_validity(&def.eval, symbol_table.clone())?;
    }
    check_symbol_validity(ast, symbol_table)?;

    let data = build_type_tree(ast.clone(), &mut fresh);

    let upper = TypedAst::Let {
        bindings: definitions
            .iter()
            .map(|def| (def.name, build_type_tree(def.eval.clone(), &mut fresh)))
            .collect(),
        span: *data.span(),
        ty: Type::Var(fresh.next(), *data.span()),
        expr: Box::new(data),
    };

    // let (program_sub, ty) = infer(&env, &mut fresh, &upper)?;

    // INFER 2

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

    env.map.insert(
        "if",
        parse_type_scheme(Span::new_extra("fn(bool, T, T) -> T", "<internal>"))
            .unwrap()
            .1,
    );

    for def in definitions {
        check_symbol_validity(&def.eval, symbol_table.clone())?;
    }
    check_symbol_validity(ast, symbol_table)?;

    let data = build_type_tree(ast.clone(), &mut fresh);

    let upper = TypedAst::Let {
        bindings: definitions
            .iter()
            .map(|def| (def.name, build_type_tree(def.eval.clone(), &mut fresh)))
            .collect(),
        span: *data.span(),
        ty: Type::Var(fresh.next(), *data.span()),
        expr: Box::new(data),
    };

    let mut unifiers = Default::default();
    infer2(&mut unifiers, &env, &mut fresh, &upper);

    for s in unifiers.iter() {
        println!("{} = {}", s.expected, s.found);
    }

    println!("----");

    let (sub2, errors) = unify2(unifiers);

    if !errors.is_empty() {
        return Err(CompileError::List(
            errors.into_iter().map(|val| val.into()).collect(),
        ));
    }

    for s in sub2.map.iter() {
        println!("{} = {}", s.0, s.1);
    }

    // ty.apply(&program_sub);

    // env.apply(&program_sub);

    Ok({
        let mut upper = upper;
        let mut upper2 = upper.clone();
        // upper.apply(&program_sub);
        upper2.apply(&sub2);
        // println!("{}", upper);
        // println!("{}", upper2);
        upper
    })
}
