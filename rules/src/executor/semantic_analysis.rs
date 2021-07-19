pub(crate) mod hm;
mod symbol_validity;

use std::collections::HashSet;

use crate::{
    executor::{
        error::CompileError,
        semantic_analysis::{
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

    let (program_sub, ty) = infer(&env, &mut fresh, &upper)?;

    ty.apply(&program_sub);

    env.apply(&program_sub);

    Ok({
        let mut upper = upper;
        println!("{}", program_sub);
        println!("{}", upper);
        upper.apply(&program_sub);
        println!("{}", upper);
        upper
    })
}
