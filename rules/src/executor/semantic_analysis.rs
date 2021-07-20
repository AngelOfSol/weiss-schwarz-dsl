pub(crate) mod constraints;
pub(crate) mod hm;
mod symbol_validity;

use std::collections::HashSet;

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
    infer(&mut unifiers, &env, &mut fresh, &upper);

    let (program_substitutions, unification_errors) = unify(unifiers);

    if !unification_errors.is_empty() {
        return Err(CompileError::List(
            unification_errors
                .into_iter()
                .map(CompileError::from)
                .collect(),
        ));
    }

    Ok({
        let mut upper = upper;
        upper.apply(&program_substitutions);
        upper
    })
}
