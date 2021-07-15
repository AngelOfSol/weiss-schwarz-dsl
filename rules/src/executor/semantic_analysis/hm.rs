use std::{collections::HashSet, iter::once};

use crate::{
    executor::{
        error::CompileError,
        semantic_analysis::hm::{
            substitution::Substitution,
            type_environment::TypeEnvironment,
            type_schemes::TypeScheme,
            types::{Type, TypeName, TypeVariable},
        },
    },
    parsing::{SexprValue, Span},
};

pub mod substitution;
pub mod type_environment;
pub mod type_schemes;
pub mod types;

#[derive(Debug, Clone)]
enum TypeTree<'a> {
    Call {
        children: Vec<TypeTree<'a>>,
        span: Span<'a>,
    },

    Let {
        bindings: Vec<(&'a str, TypeTree<'a>)>,
        expr: Box<TypeTree<'a>>,
        span: Span<'a>,
    },
    Fn {
        bindings: Vec<&'a str>,
        expr: Box<TypeTree<'a>>,
        span: Span<'a>,
    },
    Binding {
        name: &'a str,
    },
    Leaf(Type),
}

impl<'a> TypeTree<'a> {
    #[allow(dead_code)]
    fn apply(&mut self, rules: &Substitution) {
        match self {
            TypeTree::Call { children, .. } => {
                for child in children {
                    child.apply(rules);
                }
            }
            TypeTree::Let { bindings, expr, .. } => {
                for (_, ty) in bindings {
                    ty.apply(rules);
                }
                expr.apply(rules);
            }
            TypeTree::Binding { .. } => (),
            TypeTree::Leaf(ty) => *ty = ty.apply(rules),
            TypeTree::Fn { expr, .. } => {
                expr.apply(rules);
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct Fresh(usize);
impl Fresh {
    fn next(&mut self) -> TypeVariable {
        let ret = self.0;
        self.0 += 1;
        TypeVariable::new(ret)
    }
}
fn unify(lhs: Type, rhs: Type, span: Span) -> Result<Substitution, CompileError> {
    match (&lhs, &rhs) {
        (
            Type::Constant {
                name: lhs_name,
                parameters: lhs_params,
            },
            Type::Constant {
                name: rhs_name,
                parameters: rhs_params,
            },
        ) => {
            let mut failed = false;

            if lhs_name != rhs_name {
                failed = true;
            }
            let mut sub = Substitution::default();

            for (left, right) in lhs_params.iter().zip(rhs_params.iter()) {
                match unify(left.apply(&sub), right.apply(&sub), span) {
                    Ok(applied) => sub = sub.union(applied),
                    Err(_) => {
                        failed = true;
                    }
                }
            }
            if failed {
                Err(CompileError::InvalidHMType {
                    expected: lhs.apply(&sub),
                    found: rhs.apply(&sub),
                    value: span.data,
                })
            } else {
                Ok(sub)
            }
        }
        (any, Type::Var(tvar)) | (Type::Var(tvar), any) => {
            if any == &Type::Var(*tvar) {
                Ok(Substitution::default())
            } else {
                Ok(Substitution {
                    map: maplit::hashmap! {
                        *tvar => any.clone()
                    },
                })
            }
        }
    }
}

fn infer<'a>(
    env: &TypeEnvironment<'a>,
    fresh: &mut Fresh,
    tt: &TypeTree<'a>,
) -> Result<(Substitution, Type), CompileError<'a>> {
    match tt {
        TypeTree::Call { children, span, .. } => {
            let fresh_type_variable = Type::Var(fresh.next());

            let (mut sub, fn_type) = infer(env, fresh, &children[0])?;

            let types = children[1..]
                .iter()
                .map(|child| {
                    let (new_sub, ty) = infer(env, fresh, child)?;
                    let ty = ty.apply(&sub);
                    sub = sub.clone().union(new_sub);
                    Ok(ty)
                })
                .chain(once(Ok(fresh_type_variable.clone())))
                .collect::<Result<Vec<_>, _>>()?;

            let unified = unify(
                fn_type,
                Type::Constant {
                    name: TypeName::Fn,
                    parameters: types,
                },
                *span,
            )?;

            let ty = fresh_type_variable.apply(&unified);

            Ok((sub.union(unified), ty))
        }
        TypeTree::Let { bindings, expr, .. } => {
            let mut sub = Substitution::default();
            let mut env = env.apply(&sub);

            for (name, value) in bindings {
                let (new_sub, value) = infer(&mut env, fresh, value)?;
                let t_prime = env.generalize(value);
                sub = sub.union(new_sub);

                env.map.insert(*name, t_prime);
            }

            let expr = infer(&mut env, fresh, expr)?;

            Ok((sub.union(expr.0), expr.1))
        }
        TypeTree::Binding { name } => Ok((Substitution::default(), {
            env.map.get(*name).unwrap().new_vars(fresh)
        })),
        TypeTree::Leaf(ty) => Ok((Substitution::default(), ty.clone())),
        TypeTree::Fn { bindings, expr, .. } => {
            let mut env = env.apply(&Substitution::default());

            let parameters = bindings
                .iter()
                .map(|binding| {
                    let var = Type::Var(fresh.next());
                    env.map.insert(
                        binding,
                        TypeScheme {
                            ty: var.clone(),
                            type_variables: HashSet::new(),
                        },
                    );
                    var
                })
                .collect::<Vec<_>>();

            let (sub, ty) = infer(&mut env, fresh, expr)?;

            let parameters = parameters
                .into_iter()
                .map(|ty| ty.apply(&sub))
                .chain(once(ty))
                .collect();

            Ok((
                sub,
                Type::Constant {
                    name: TypeName::Fn,
                    parameters,
                },
            ))
        }
    }
}

fn build_type_tree<'a>(sexpr: &SexprValue<'a>, fresh: &mut Fresh) -> TypeTree<'a> {
    match sexpr {
        SexprValue::Sexpr(inner) => TypeTree::Call {
            children: vec![TypeTree::Binding { name: inner.target }]
                .into_iter()
                .chain(
                    inner
                        .arguments
                        .iter()
                        .map(|arg| build_type_tree(arg, fresh)),
                )
                .collect(),
            span: inner.span,
        },
        SexprValue::Symbol(binding) => TypeTree::Binding { name: *binding },
        SexprValue::Integer(_) => TypeTree::Leaf(Type::Constant {
            name: TypeName::Integer,
            parameters: vec![],
        }),
        SexprValue::Bool(_) => TypeTree::Leaf(Type::Constant {
            name: TypeName::Bool,
            parameters: vec![],
        }),
        SexprValue::Zone(_) => TypeTree::Leaf(Type::Constant {
            name: TypeName::Zone,
            parameters: vec![],
        }),
        SexprValue::Unit(_) => TypeTree::Leaf(Type::Constant {
            name: TypeName::Unit,
            parameters: vec![],
        }),
        SexprValue::None => TypeTree::Leaf(Type::Constant {
            name: TypeName::Option,
            parameters: vec![Type::Var(fresh.next())],
        }),
        SexprValue::Array { .. } => todo!(),
        SexprValue::Fn {
            arguments,
            eval,
            span,
            ..
        } => TypeTree::Fn {
            bindings: arguments.iter().map(|(name, _)| *name).collect(),
            expr: Box::new(build_type_tree(eval, fresh)),
            span: *span,
        },
        SexprValue::If {
            condition,
            if_true,
            if_false,
            span,
            ..
        } => TypeTree::Call {
            children: vec![
                TypeTree::Binding { name: "if" },
                build_type_tree(condition, fresh),
                build_type_tree(if_true, fresh),
                build_type_tree(if_false, fresh),
            ],
            span: *span,
        },
        SexprValue::Let {
            bindings,
            expr,
            span,
            ..
        } => TypeTree::Let {
            bindings: bindings
                .iter()
                .map(|(key, value)| (*key, build_type_tree(value, fresh)))
                .collect(),
            expr: Box::new(build_type_tree(expr, fresh)),
            span: *span,
        },
    }
}

pub fn type_check<'a>(ast: &'a SexprValue) -> Result<Type, CompileError<'a>> {
    let mut fresh = Fresh::default();
    let mut env = TypeEnvironment::default();

    let idx = fresh.next();
    let s_var = Type::Var(idx);
    env.map.insert(
        "if",
        TypeScheme {
            type_variables: maplit::hashset! { idx },
            ty: Type::Constant {
                name: TypeName::Fn,
                parameters: vec![
                    Type::Constant {
                        name: TypeName::Bool,
                        parameters: vec![],
                    },
                    s_var.clone(),
                    s_var.clone(),
                    s_var.clone(),
                ],
            },
        },
    );
    for (key, value) in crate::executor::RUST_FN_TYPE_SCHEMES.iter() {
        env.map.insert(key, value.clone());
    }

    let data = build_type_tree(ast, &mut fresh);

    let (_, ty) = infer(&env, &mut fresh, &data)?;

    Ok(ty)
}
