use std::{collections::BTreeSet, iter::once};

use crate::executor::{
    error::TypeError,
    semantic_analysis::hm::{
        substitution::Substitution, type_environment::TypeEnvironment, type_schemes::TypeScheme,
        types::Type,
    },
};

pub mod fresh;
mod substitution;
pub mod type_environment;
pub mod type_schemes;
pub mod type_tree;
pub mod types;
pub mod unify;

pub(crate) use fresh::Fresh;
pub(crate) use type_tree::TypedAst;
use unify::unify;

pub(crate) fn infer<'a>(
    env: &TypeEnvironment<'a>,
    fresh: &mut Fresh,
    tt: &TypedAst<'a>,
) -> Result<(Substitution<'a>, Type<'a>), TypeError<'a>> {
    match tt {
        TypedAst::Call { children, span, .. } => {
            let fresh_type_variable = Type::Var(fresh.next(), *span);

            let (mut sub, fn_type) = infer(env, fresh, &children[0])?;

            let mut types = children[1..]
                .iter()
                .map(|child| {
                    let (new_sub, ty) = infer(env, fresh, child)?;
                    let ty = ty.apply(&sub);
                    sub = sub.clone().union(new_sub);
                    Ok(ty)
                })
                .collect::<Result<Vec<_>, _>>()?;
            types.push(fresh_type_variable.apply(&sub));

            let unified = unify(fn_type, Type::function(types, *span))?;

            let ty = fresh_type_variable.apply(&unified);

            Ok((sub.union(unified), ty))
        }
        TypedAst::Let { bindings, expr, .. } => {
            let mut sub = Substitution::default();
            let mut env = env.clone();

            for (name, inner) in bindings {
                let fresh_type_variable = Type::type_var(fresh.next(), *inner.span());

                env.map.insert(
                    *name,
                    TypeScheme {
                        ty: fresh_type_variable,
                        quantified_variables: BTreeSet::new(),
                    },
                );
            }

            for (name, value) in bindings {
                let (new_sub, value) = infer(&mut env, fresh, value)?;

                let unified_sub = unify(env.map[name].ty.clone(), value.clone())?;

                // we apply the substitutions from the inference here
                env = env.apply(&new_sub);

                // so that when we generalize here,
                // the quantified vs free variables are correct
                let t_prime = env.generalize(value);

                // afterwards we apply the unification to
                // backfill on to previous inferences
                env = env.apply(&unified_sub);

                // then we insert the proper generalized version
                // into the environment
                env.map.insert(*name, t_prime);

                sub = sub.union(new_sub);
                sub = sub.union(unified_sub);
            }

            let (expr, ty) = infer(&mut env, fresh, expr)?;

            Ok((sub.union(expr), ty))
        }
        TypedAst::Binding { name, span } => Ok((Substitution::default(), {
            env.map.get(*name).unwrap().new_vars(fresh).with_span(*span)
        })),
        TypedAst::Value { ty, .. } => Ok((Substitution::default(), ty.clone())),
        TypedAst::Fn {
            bindings,
            expr,
            span,
            return_type,
            ..
        } => {
            let mut env = env.clone();

            let decl_ty = Type::function(
                bindings
                    .iter()
                    .map(|(_, lhs)| lhs.clone())
                    .chain(once(return_type.clone()))
                    .collect(),
                *span,
            );

            let decl_remap = decl_ty
                .free_variables()
                .into_iter()
                .map(|old| (old, fresh.next()))
                .collect();

            let decl_ty = decl_ty.remap(&decl_remap);

            let parameters = bindings
                .iter()
                .map(|(binding, ty)| {
                    let fresh_type_variable = Type::type_var(fresh.next(), *ty.span());

                    env.map.insert(
                        binding,
                        TypeScheme {
                            ty: fresh_type_variable.clone(),
                            quantified_variables: BTreeSet::new(),
                        },
                    );

                    fresh_type_variable
                })
                .collect::<Vec<_>>();

            let (sub, ty) = infer(&mut env, fresh, expr)?;

            let parameters = parameters
                .into_iter()
                .map(|ty| ty.apply(&sub))
                .chain(once(ty.apply(&sub)))
                .collect::<Vec<_>>();

            let result_ty = Type::function(parameters.clone(), *ty.span());

            // this checks to make sure that our declaration and our inferred types match properly
            let new_sub = unify(decl_ty, result_ty.clone())?;

            let result_ty = result_ty.apply(&new_sub);

            Ok((sub.union(new_sub), result_ty))
        }
        TypedAst::Seq {
            sub_expressions, ..
        } => {
            let mut sub = Substitution::default();
            let mut resulting_types = sub_expressions
                .iter()
                .map(|expr| {
                    let (new_sub, ty) = infer(env, fresh, expr)?;
                    sub = sub.clone().union(new_sub);
                    Ok(ty)
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok((sub, resulting_types.pop().unwrap()))
        }
        TypedAst::Array { values, span } => {
            let mut sub = Substitution::default();
            let mut resulting_types = values
                .iter()
                .map(|expr| {
                    let (new_sub, ty) = infer(env, fresh, expr)?;
                    sub = sub.clone().union(new_sub);
                    Ok(ty)
                })
                .collect::<Result<BTreeSet<_>, _>>()?;

            if resulting_types.len() == 1 {
                let ty = resulting_types.pop_first().unwrap();
                Ok((sub, Type::array(ty, *span)))
            } else {
                Err(TypeError::InvalidArray {
                    found: resulting_types,
                    span: *span,
                })
            }
        }
        TypedAst::If {
            condition,
            if_true,
            if_false,
            ..
        } => {
            let mut sub = Substitution::default();

            let (condition_sub, _) = infer(env, fresh, condition)?;

            sub = sub.union(condition_sub);

            let (true_sub, if_true) = infer(env, fresh, if_true)?;
            sub = sub.union(true_sub);
            let (false_sub, if_false) = infer(env, fresh, if_false)?;
            sub = sub.union(false_sub);

            let unified = unify(if_true.clone(), if_false)?;

            sub = sub.union(unified);

            Ok((sub, if_true))
        }
    }
}
