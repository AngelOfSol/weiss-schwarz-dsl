use std::{collections::HashSet, iter::once};

use crate::{
    executor::{
        error::{CompileError, TypeError},
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
        bindings: Vec<(&'a str, Type<'a>)>,
        expr: Box<TypeTree<'a>>,
        span: Span<'a>,
    },
    Binding {
        name: &'a str,
        span: Span<'a>,
    },
    Leaf(Type<'a>),
}

impl<'a> TypeTree<'a> {
    #[allow(dead_code)]
    fn apply(&mut self, rules: &Substitution<'a>) {
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
    pub fn next(&mut self) -> TypeVariable {
        let ret = self.0;
        self.0 += 1;
        TypeVariable::new(ret)
    }
}
fn unify<'a>(lhs: Type<'a>, rhs: Type<'a>) -> Result<Substitution<'a>, TypeError<'a>> {
    match (&lhs, &rhs) {
        (
            Type::Constant {
                name: lhs_name,
                parameters: lhs_params,
                ..
            },
            Type::Constant {
                name: rhs_name,
                parameters: rhs_params,
                span: rhs_span,
            },
        ) => {
            let mut err = None;

            if lhs_name != rhs_name || lhs_params.len() != rhs_params.len() {
                err = Some(TypeError::InvalidType {
                    expected: lhs.clone(),
                    found: rhs.clone(),
                    span: *rhs_span,
                });
            }
            let mut sub = Substitution::default();

            for (left, right) in lhs_params.iter().zip(rhs_params.iter()) {
                match unify(left.apply(&sub), right.apply(&sub)) {
                    Ok(applied) => sub = sub.union(applied),
                    Err(_) => {
                        let span = *right.span();

                        err = Some(TypeError::InvalidType {
                            expected: left.clone(),
                            found: right.clone(),
                            span,
                        });
                    }
                }
            }
            if let Some(err) = err {
                match err {
                    TypeError::InvalidType {
                        expected,
                        found,
                        span,
                    } => Err(TypeError::InvalidType {
                        expected: expected.apply(&sub),
                        found: found.apply(&sub),
                        span,
                    }),
                    err => Err(err),
                }
            } else {
                Ok(sub)
            }
        }
        (Type::Var(tvar, span), rhs @ Type::Var(rhs_tvar, ..)) => {
            if tvar == rhs_tvar {
                Err(TypeError::InfiniteType {
                    left: Type::Var(*tvar, *span),
                    right: rhs.clone(),
                    span: *span,
                })
            } else {
                Ok(Substitution {
                    map: maplit::hashmap! {
                        *tvar => rhs.clone()
                    },
                })
            }
        }
        (any, Type::Var(tvar, span)) | (Type::Var(tvar, span), any) => {
            if any.occurs(tvar) {
                Err(TypeError::InfiniteType {
                    left: Type::Var(*tvar, *span),
                    right: any.clone(),
                    span: *span,
                })
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
) -> Result<(Substitution<'a>, Type<'a>), TypeError<'a>> {
    match tt {
        TypeTree::Call { children, span, .. } => {
            let fresh_type_variable = Type::Var(fresh.next(), *span);

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
                    span: *span,
                    name: TypeName::Fn,
                    parameters: types,
                },
            )?;

            let ty = fresh_type_variable.apply(&unified);

            Ok((sub.union(unified), ty))
        }
        TypeTree::Let { bindings, expr, .. } => {
            let mut sub = Substitution::default();
            let mut env = env.clone();

            for (name, value) in bindings {
                let (new_sub, value) = infer(&mut env, fresh, value)?;
                let t_prime = env.generalize(value);
                sub = sub.union(new_sub);

                env.map.insert(*name, t_prime);
            }

            let expr = infer(&mut env, fresh, expr)?;

            Ok((sub.union(expr.0), expr.1))
        }
        TypeTree::Binding { name, span } => Ok((Substitution::default(), {
            env.map.get(*name).unwrap().new_vars(fresh).with_span(*span)
        })),
        TypeTree::Leaf(ty) => Ok((Substitution::default(), ty.clone())),
        TypeTree::Fn {
            bindings,
            expr,
            span,
            ..
        } => {
            let mut env = env.clone();

            let parameters = bindings
                .iter()
                .map(|(binding, ty)| {
                    let var = ty.clone();
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
                    span: *span,
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
            children: vec![TypeTree::Binding {
                name: inner.target,
                span: inner.span,
            }]
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
        SexprValue::Symbol(binding, span) => TypeTree::Binding {
            name: *binding,
            span: *span,
        },
        SexprValue::Integer(_, span) => TypeTree::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Integer,
            parameters: vec![],
        }),
        SexprValue::Bool(_, span) => TypeTree::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Bool,
            parameters: vec![],
        }),
        SexprValue::Zone(_, span) => TypeTree::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Zone,
            parameters: vec![],
        }),
        SexprValue::Unit(span) => TypeTree::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Unit,
            parameters: vec![],
        }),
        SexprValue::None(span) => TypeTree::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Option,
            parameters: vec![Type::Var(fresh.next(), *span)],
        }),
        SexprValue::Array { .. } => todo!(),
        SexprValue::Fn {
            arguments,
            eval,
            span,
            ..
        } => TypeTree::Fn {
            bindings: arguments.clone(),
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
                TypeTree::Binding {
                    name: "if",
                    span: *span,
                },
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

pub fn type_check<'a>(ast: &'a SexprValue) -> Result<Type<'a>, TypeError<'a>> {
    let mut fresh = Fresh::default();
    let mut env = TypeEnvironment::default();

    let idx = fresh.next();
    let s_var = Type::Var(idx, Span::new("if"));
    env.map.insert(
        "if",
        TypeScheme {
            type_variables: maplit::hashset! { idx },
            ty: Type::Constant {
                span: Span::new("if"),
                name: TypeName::Fn,
                parameters: vec![
                    Type::Constant {
                        span: Span::new("bool"),
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

    let (_sub, ty) = infer(&env, &mut fresh, &data)?;

    let mut data = data;
    data.apply(&_sub);
    dbg!(&ty);
    // dbg!(_sub);
    // dbg!(data);
    if !ty.is_concrete() {
        Err(TypeError::UninferredType { ty })
    } else {
        Ok(ty)
    }
}
