use std::{collections::BTreeSet, iter::once};

use crate::executor::{
    error::TypeError,
    semantic_analysis::hm::{
        substitution::Substitution, type_environment::TypeEnvironment, type_schemes::TypeScheme,
        types::Type, Fresh, TypedAst,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Constraint<'a> {
    pub expected: Type<'a>,
    pub found: Type<'a>,
}

impl<'a> Constraint<'a> {
    fn apply(&mut self, sub: &Substitution<'a>) {
        self.expected = self.expected.apply(sub);
        self.found = self.found.apply(sub);
    }
}

pub(crate) fn unify<'a>(
    mut constraints: Vec<Constraint<'a>>,
) -> (Substitution<'a>, Vec<TypeError<'a>>) {
    let mut errors = vec![];
    let mut ret = Substitution::default();
    while let Some(constraint) = constraints.pop() {
        let expected = constraint.expected;
        let found = constraint.found;
        let new_sub = match (&expected, &found) {
            (
                Type::Constant {
                    name: expected_name,
                    parameters: expected_params,
                    ..
                },
                Type::Constant {
                    name: found_name,
                    parameters: found_params,
                    ..
                },
            ) => {
                if expected_name != found_name || expected_params.len() != found_params.len() {
                    errors.push(TypeError::InvalidType {
                        expected: expected.clone(),
                        found: found.clone(),
                    });
                    continue;
                }

                for (expected, found) in expected_params
                    .iter()
                    .cloned()
                    .zip(found_params.iter().cloned())
                {
                    constraints.push(Constraint { expected, found });
                }
                Substitution::default()
            }
            (Type::Var(tvar, ..), rhs @ Type::Var(rhs_tvar, ..)) => {
                if tvar == rhs_tvar {
                    Substitution::default()
                } else {
                    Substitution {
                        map: vec![(*tvar, rhs.clone())],
                    }
                }
            }
            (any, Type::Var(tvar, span)) | (Type::Var(tvar, span), any) => {
                if any.occurs(tvar) {
                    errors.push(TypeError::InfiniteType {
                        left: Type::Var(*tvar, *span),
                        right: any.clone(),
                        span: *span,
                    });
                    continue;
                } else {
                    Substitution {
                        map: vec![(*tvar, any.clone())],
                    }
                }
            }
        };

        for c in constraints.iter_mut() {
            c.apply(&new_sub);
        }

        ret = ret.union(new_sub);
    }

    (ret, errors)
}

pub(crate) fn infer<'a>(
    unifiers: &mut Vec<Constraint<'a>>,
    env: &TypeEnvironment<'a>,
    fresh: &mut Fresh,
    tt: &TypedAst<'a>,
) -> Type<'a> {
    match tt {
        TypedAst::Eval {
            children, span, ty, ..
        } => {
            let fn_ty = infer(unifiers, env, fresh, &children[0]);

            let mut types = children[1..]
                .iter()
                .map(|child| infer(unifiers, env, fresh, child))
                .collect::<Vec<_>>();
            types.push(ty.clone());

            unifiers.push(Constraint {
                expected: fn_ty,
                found: Type::function(types, *span),
            });

            ty.clone()
        }
        TypedAst::Let {
            bindings, expr, ty, ..
        } => {
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
                let value = infer(unifiers, &mut env, fresh, value);

                unifiers.push(Constraint {
                    expected: env.map[name].ty.clone(),
                    found: value.clone(),
                });

                // so that when we generalize here,
                // the quantified vs free variables are correct
                let t_prime = env.generalize(value);

                // then we insert the proper generalized version
                // into the environment
                env.map.insert(*name, t_prime);
            }

            let result_ty = infer(unifiers, &mut env, fresh, expr);

            unifiers.push(Constraint {
                expected: ty.clone(),
                found: result_ty.clone(),
            });

            ty.clone()
        }
        TypedAst::Binding { name, span, ty } => {
            unifiers.push(Constraint {
                expected: ty.clone(),
                found: env.map.get(*name).unwrap().new_vars(fresh).with_span(*span),
            });

            ty.clone()
        }
        TypedAst::Value { ty, .. } => ty.clone(),
        TypedAst::Fn {
            bindings,
            expr,
            span,
            return_type,
            ty,
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

            let inferred = infer(unifiers, &mut env, fresh, expr);

            let parameters = parameters
                .into_iter()
                .chain(once(inferred))
                .collect::<Vec<_>>();

            let result_ty = Type::function(parameters.clone(), *expr.span());

            unifiers.extend(vec![
                // this checks to make sure that our declaration and our inferred types match properly
                Constraint {
                    expected: decl_ty,
                    found: result_ty.clone(),
                },
                Constraint {
                    expected: ty.clone(),
                    found: result_ty,
                },
            ]);

            ty.clone()
        }
        TypedAst::Seq {
            sub_expressions,
            ty,
            ..
        } => {
            let seq = Constraint {
                expected: ty.clone(),
                found: sub_expressions
                    .iter()
                    .map(|expr| infer(unifiers, env, fresh, expr))
                    .last()
                    .unwrap(),
            };
            unifiers.push(seq);

            ty.clone()
        }
        TypedAst::Array { values, span, ty } => {
            let fresh_type_variable = Type::type_var(fresh.next(), *span);

            let values = values
                .iter()
                .rev()
                .map(|value| Constraint {
                    expected: fresh_type_variable.clone().with_span(*value.span()),
                    found: infer(unifiers, env, fresh, value),
                })
                .collect::<Vec<_>>();

            unifiers.extend(values);

            unifiers.push(Constraint {
                expected: ty.clone(),
                found: Type::array(fresh_type_variable, *span),
            });

            ty.clone()
        }
        TypedAst::If {
            condition,
            if_true,
            if_false,
            ty,
            ..
        } => {
            let condition_ty = infer(unifiers, env, fresh, condition);
            let if_true_ty = infer(unifiers, env, fresh, if_true);
            let if_false_ty = infer(unifiers, env, fresh, if_false);

            unifiers.push(Constraint {
                expected: Type::boolean(*condition.span()),
                found: condition_ty,
            });
            unifiers.push(Constraint {
                expected: ty.clone().with_span(*if_true.span()),
                found: if_false_ty.clone(),
            });
            unifiers.push(Constraint {
                expected: ty.clone().with_span(*if_true.span()),
                found: if_true_ty.clone(),
            });

            ty.clone()
        }
    }
}
