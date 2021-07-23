use std::iter::once;

use crate::executor::{
    error::TypeError,
    semantic_analysis::hm::{
        substitution::Substitution, type_environment::TypeEnvironment, type_schemes::TypeScheme,
        types::Type, Fresh, TypedAst,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Constraint {
    pub expected: Type,
    pub found: Type,
}

impl Constraint {
    fn apply(mut self, sub: &Substitution) -> Self {
        self.expected = self.expected.apply(sub);
        self.found = self.found.apply(sub);

        self
    }
}

pub(crate) fn unify(mut constraints: Vec<Constraint>) -> (Substitution, Vec<TypeError>) {
    let mut errors = vec![];
    let mut ret = Substitution::default();

    while let Some(constraint) = constraints.pop() {
        let expected = constraint.expected;
        let found = constraint.found;

        if &expected == &found {
            continue;
        }

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
                    .rev()
                    .cloned()
                    .zip(found_params.iter().rev().cloned())
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
                        left: Type::Var(*tvar, span.clone()),
                        right: any.clone(),
                        span: span.clone(),
                    });
                    continue;
                } else {
                    Substitution {
                        map: vec![(*tvar, any.clone())],
                    }
                }
            }
        };

        constraints = constraints.into_iter().map(|c| c.apply(&new_sub)).collect();

        ret = new_sub.union(ret);
    }

    (ret, errors)
}

pub(crate) fn infer(
    unifiers: &mut Vec<Constraint>,
    type_environment: &TypeEnvironment,
    fresh: &mut Fresh,
    tt: &TypedAst,
) -> Type {
    match tt {
        TypedAst::Eval {
            children, span, ty, ..
        } => {
            let fn_ty = infer(unifiers, type_environment, fresh, &children[0]);

            let mut types = children[1..]
                .iter()
                .map(|child| infer(unifiers, type_environment, fresh, child))
                .collect::<Vec<_>>();
            types.push(ty.clone());

            let found = Type::function(types, span.clone());

            unifiers.push(Constraint {
                expected: fn_ty,
                found,
            });

            ty.clone()
        }
        TypedAst::Let {
            bindings, expr, ty, ..
        } => {
            let mut type_environment = type_environment.clone();

            for (name, inner) in bindings {
                let fresh_type_variable =
                    Type::type_var(fresh.next_type_variable(), inner.span().clone());

                type_environment.map.insert(
                    name.clone(),
                    TypeScheme {
                        ty: fresh_type_variable,
                        quantified_variables: Default::default(),
                    },
                );
            }

            for (name, value) in bindings {
                let mut new_unifiers = vec![];
                let value = infer(&mut new_unifiers, &type_environment, fresh, value);

                // we can ignore the errors here, becuase the top level unification will find them
                let (subs, _) = unify(new_unifiers.clone());

                unifiers.extend(new_unifiers);

                // we apply the substitutions from the inference here
                type_environment.apply(&subs);

                // so that when we generalize here,
                // the quantified vs free variables are correct
                let t_prime = type_environment.generalize(value);

                // then we insert the proper generalized version
                // into the environment
                type_environment.map.insert(name.clone(), t_prime);
            }

            let result_ty = infer(unifiers, &mut type_environment, fresh, expr);

            unifiers.push(Constraint {
                expected: ty.clone(),
                found: result_ty.clone(),
            });

            result_ty
        }
        TypedAst::Binding { name, span, ty } => {
            let found = type_environment
                .map
                .get(name)
                .unwrap()
                .new_vars(fresh)
                .with_span(span.clone());

            unifiers.push(Constraint {
                expected: ty.clone(),
                found: found.clone(),
            });

            found
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
            let mut type_environment = type_environment.clone();

            let decl_ty = Type::function(
                bindings
                    .iter()
                    .map(|(_, lhs)| lhs.clone())
                    .chain(once(return_type.clone()))
                    .collect(),
                span.clone(),
            );

            let parameters = bindings
                .iter()
                .map(|(binding, ty)| {
                    let fresh_type_variable =
                        Type::type_var(fresh.next_type_variable(), ty.span().clone());

                    type_environment.map.insert(
                        binding.clone(),
                        TypeScheme {
                            ty: fresh_type_variable.clone(),
                            quantified_variables: Default::default(),
                        },
                    );

                    fresh_type_variable
                })
                .collect::<Vec<_>>();

            let inferred = infer(unifiers, &mut type_environment, fresh, expr);

            let parameters = parameters
                .into_iter()
                .chain(once(inferred))
                .collect::<Vec<_>>();

            let result_ty = Type::function(parameters.clone(), expr.span().clone());

            unifiers.extend(vec![
                // this checks to make sure that our declaration and our inferred types match properly
                Constraint {
                    expected: decl_ty.clone(),
                    found: result_ty.clone(),
                },
                Constraint {
                    expected: ty.clone(),
                    found: result_ty.clone(),
                },
            ]);

            decl_ty
        }
        TypedAst::Seq {
            sub_expressions,
            ty,
            ..
        } => {
            let result_ty = sub_expressions
                .iter()
                .map(|expr| infer(unifiers, type_environment, fresh, expr))
                .last()
                .unwrap();
            unifiers.push(Constraint {
                expected: ty.clone(),
                found: result_ty.clone(),
            });

            result_ty
        }
        TypedAst::Array { values, span, ty } => {
            let fresh_type_variable = Type::type_var(fresh.next_type_variable(), span.clone());

            let values = values
                .iter()
                .rev()
                .map(|value| Constraint {
                    expected: fresh_type_variable.clone().with_span(value.span().clone()),
                    found: infer(unifiers, type_environment, fresh, value),
                })
                .collect::<Vec<_>>();

            unifiers.extend(values);

            let result_ty = Type::array(fresh_type_variable, span.clone());
            unifiers.push(Constraint {
                expected: ty.clone(),
                found: result_ty.clone(),
            });

            result_ty
        }
        TypedAst::If {
            condition,
            if_true,
            if_false,
            ty,
            ..
        } => {
            let condition_ty = infer(unifiers, type_environment, fresh, condition);
            let if_true_ty = infer(unifiers, type_environment, fresh, if_true);
            let if_false_ty = infer(unifiers, type_environment, fresh, if_false);

            unifiers.push(Constraint {
                expected: Type::boolean(condition.span().clone()),
                found: condition_ty,
            });
            unifiers.push(Constraint {
                expected: ty.clone().with_span(if_true.span().clone()),
                found: if_false_ty.clone(),
            });
            unifiers.push(Constraint {
                expected: ty.clone().with_span(if_true.span().clone()),
                found: if_true_ty.clone(),
            });

            if_true_ty
        }
    }
}
