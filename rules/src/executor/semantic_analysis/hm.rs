use std::{collections::BTreeSet, iter::once};

use crate::{
    executor::{
        error::TypeError,
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
pub(crate) enum TypeTree<'a> {
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
        return_type: Type<'a>,
        expr: Box<TypeTree<'a>>,
        span: Span<'a>,
    },
    Binding {
        name: &'a str,
        span: Span<'a>,
    },
    Seq {
        sub_expressions: Vec<TypeTree<'a>>,
        span: Span<'a>,
    },
    Leaf(Type<'a>),
}

impl<'a> TypeTree<'a> {
    pub(crate) fn span(&self) -> &Span<'a> {
        match self {
            TypeTree::Call { span, .. } => span,
            TypeTree::Let { span, .. } => span,
            TypeTree::Fn { span, .. } => span,
            TypeTree::Binding { span, .. } => span,
            TypeTree::Seq { span, .. } => span,
            TypeTree::Leaf(inner) => inner.span(),
        }
    }
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
            TypeTree::Seq {
                sub_expressions, ..
            } => {
                for expr in sub_expressions {
                    expr.apply(rules);
                }
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
                    Err(bubbled) => {
                        err = Some(bubbled);
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
        (Type::Var(tvar, ..), rhs @ Type::Var(rhs_tvar, ..)) => {
            if tvar == rhs_tvar {
                Ok(Substitution::default())
            } else {
                Ok(Substitution {
                    map: vec![(*tvar, rhs.clone())],
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
                    map: vec![(*tvar, any.clone())],
                })
            }
        }
    }
}

pub(crate) fn infer<'a>(
    env: &TypeEnvironment<'a>,
    fresh: &mut Fresh,
    tt: &TypeTree<'a>,
) -> Result<(Substitution<'a>, Type<'a>), TypeError<'a>> {
    match tt {
        TypeTree::Call { children, span, .. } => {
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
        TypeTree::Let { bindings, expr, .. } => {
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
        TypeTree::Binding { name, span } => Ok((Substitution::default(), {
            env.map.get(*name).unwrap().new_vars(fresh).with_span(*span)
        })),
        TypeTree::Leaf(ty) => Ok((Substitution::default(), ty.clone())),
        TypeTree::Fn {
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
        TypeTree::Seq {
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
    }
}

pub(crate) fn build_type_tree<'a>(sexpr: &SexprValue<'a>, fresh: &mut Fresh) -> TypeTree<'a> {
    match sexpr {
        SexprValue::Sexpr {
            target,
            span,
            arguments,
        } => TypeTree::Call {
            children: vec![TypeTree::Binding {
                name: target,
                span: *span,
            }]
            .into_iter()
            .chain(arguments.iter().map(|arg| build_type_tree(arg, fresh)))
            .collect(),
            span: *span,
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
            return_type,
            ..
        } => TypeTree::Fn {
            bindings: arguments.clone(),
            return_type: return_type.clone(),
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
        SexprValue::Seq {
            sub_expressions,
            span,
        } => TypeTree::Seq {
            sub_expressions: sub_expressions
                .iter()
                .map(|expr| build_type_tree(expr, fresh))
                .collect(),
            span: *span,
        },
    }
}

// pub fn type_check<'a>(
//     ast: &'a SexprValue,
//     externs: &Vec<ExternDeclaration<'a>>,
// ) -> Result<Type<'a>, TypeError<'a>> {
//     let data = build_type_tree(ast, &mut fresh);

//     let (_sub, ty) = infer(&env, &mut fresh, &data)?;

//     let mut data = data;
//     data.apply(&_sub);
//     // dbg!(_sub);
//     // dbg!(data);
//     if !ty.is_concrete() {
//         Err(TypeError::UninferredType { ty })
//     } else {
//         Ok(ty)
//     }
// }
