use std::{collections::HashSet, iter::once};

use crate::{
    executor::semantic_analysis::hm::{
        substitution::Substitution,
        type_environment::TypeEnvironment,
        type_schemes::TypeScheme,
        types::{Type, TypeName, TypeVariable},
    },
    parsing::SexprValue,
};

mod substitution;
mod type_environment;
mod type_schemes;
mod types;

#[derive(Debug, Clone)]
enum TypeTree<'a> {
    Call {
        children: Vec<TypeTree<'a>>,
    },

    Let {
        bindings: Vec<(&'a str, TypeTree<'a>)>,
        expr: Box<TypeTree<'a>>,
    },
    Fn {
        bindings: Vec<&'a str>,
        expr: Box<TypeTree<'a>>,
    },
    Binding {
        name: &'a str,
    },
    // Fn {
    //     names: Vec<(&'a str, Type)>,
    //     eval: Box<TypeTree<'a>>,
    //     ty: Type,
    // },
    Leaf(Type),
}

impl<'a> TypeTree<'a> {
    fn apply(&mut self, rules: &Substitution) {
        match self {
            TypeTree::Call { children } => {
                for child in children {
                    child.apply(rules);
                }
            }
            TypeTree::Let { bindings, expr } => {
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
        //
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
fn unify(lhs: Type, rhs: Type) -> Result<Substitution, String> {
    match (lhs, rhs) {
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
            if lhs_name != rhs_name {
                return Err(format!(
                    "wrong monotype: {{ {:?}, {:?} }} != {{ {:?}, {:?} }}",
                    lhs_name, lhs_params, rhs_name, rhs_params
                ));
            }
            let mut sub = Substitution::default();

            for (left, right) in lhs_params.iter().zip(rhs_params.iter()) {
                let applied = unify(left.apply(&sub), right.apply(&sub))?;
                sub = sub.union(applied);
            }
            Ok(sub)
        }
        (any, Type::Var(tvar)) | (Type::Var(tvar), any) => {
            if any == Type::Var(tvar) {
                Ok(Substitution::default())
            } else {
                Ok(Substitution {
                    map: maplit::hashmap! {
                        tvar => any
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
) -> Result<(Substitution, Type), String> {
    match tt {
        TypeTree::Call { children } => {
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
                .collect::<Result<Vec<_>, String>>()?;

            let unified = unify(
                fn_type,
                dbg!(Type::Constant {
                    name: TypeName::Fn,
                    parameters: types,
                }),
            )?;

            let ty = fresh_type_variable.apply(&unified);

            Ok((sub.union(unified), ty))
        }
        TypeTree::Let { bindings, expr } => {
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
            env.map
                .get(*name)
                .ok_or(format!("missing binding: {}", name))?
                .new_vars(fresh)
        })),
        TypeTree::Leaf(ty) => Ok((Substitution::default(), ty.clone())),
        TypeTree::Fn { bindings, expr } => {
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
        SexprValue::Array(_) => todo!(),
        SexprValue::Fn {
            arguments, eval, ..
        } => TypeTree::Fn {
            bindings: arguments.iter().map(|(name, _)| *name).collect(),
            expr: Box::new(build_type_tree(eval, fresh)),
        },
        SexprValue::If {
            condition,
            if_true,
            if_false,
        } => TypeTree::Call {
            children: vec![
                TypeTree::Binding { name: "if" },
                build_type_tree(condition, fresh),
                build_type_tree(if_true, fresh),
                build_type_tree(if_false, fresh),
            ],
        },
        SexprValue::Let { bindings, expr } => TypeTree::Let {
            bindings: bindings
                .iter()
                .map(|(key, value)| (*key, build_type_tree(value, fresh)))
                .collect(),
            expr: Box::new(build_type_tree(expr, fresh)),
        },
    }
}

#[test]
fn my_test() {
    let mut fresh = Fresh::default();
    let mut env = TypeEnvironment::default();

    let idx = fresh.next();
    let s_var = Type::Var(idx);
    env.map.insert(
        "my_test",
        TypeScheme {
            type_variables: maplit::hashset! { idx },
            ty: Type::Constant {
                name: TypeName::Fn,
                parameters: vec![s_var.clone(), s_var.clone(), s_var.clone(), s_var.clone()],
            },
        },
    );
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

    let code = "(if true true 2)";
    let parsed = crate::parsing::parse_sexpr_value(code).unwrap().1;

    let mut data = build_type_tree(&parsed, &mut fresh);

    let sub_data = infer(&env, &mut fresh, &data).unwrap();

    dbg!(&sub_data);

    data.apply(&sub_data.0);

    dbg!(data);

    panic!();
    //
}
/*
let mut data = TypeTree::Let {
        bindings: vec![(
            "x",
            TypeTree::Fn {
                bindings: vec!["a", "b", "c"],
                expr: Box::new(TypeTree::Binding { name: "b" }),
            },
        )],
        expr: Box::new(TypeTree::Let {
            bindings: vec![(
                "y",
                TypeTree::Leaf(Type::Constant {
                    name: TypeName::Integer,
                    parameters: vec![],
                }),
            )],
            expr: Box::new(TypeTree::Call {
                children: vec![
                    TypeTree::Binding { name: "my_test" },
                    TypeTree::Call {
                        children: vec![
                            TypeTree::Binding { name: "my_test" },
                            TypeTree::Leaf(Type::Constant {
                                name: TypeName::Integer,
                                parameters: vec![],
                            }),
                            TypeTree::Leaf(Type::Constant {
                                name: TypeName::Integer,
                                parameters: vec![],
                            }),
                            TypeTree::Leaf(Type::Constant {
                                name: TypeName::Integer,
                                parameters: vec![],
                            }),
                        ],
                    },
                    TypeTree::Leaf(Type::Constant {
                        name: TypeName::Integer,
                        parameters: vec![],
                    }),
                    TypeTree::Leaf(Type::Constant {
                        name: TypeName::Integer,
                        parameters: vec![],
                    }),
                ],
            }),
        }),
    };
*/
// use std::{collections::HashMap, iter::once};

// use crate::{
//     executor::value::ValueType,
//     executor::{error::CompileError, semantic_analysis::TypeCheckTable, Executor, FnTypeInfo},
//     parsing::{Sexpr, SexprValue},
// };

// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// enum MonoType {
//     Integer,
//     Bool,
//     Option,
//     Array,
//     Zone,
//     Fn,
//     Unit,
// }

// #[derive(Clone, Debug, PartialEq, Eq)]
// struct NamedTy<Type> {
//     pub name: MonoType,
//     pub type_vars: Vec<Type>,
// }

// #[derive(Clone, Debug, PartialEq, Eq)]
// enum UnboundType {
//     Named(NamedTy<UnboundType>),
//     // a -> b -> c
//     TypeVar(usize),
//     UnboundNamed(usize),
// }

// #[derive(Clone, Debug, PartialEq, Eq)]
// enum BoundType {
//     Named(NamedTy<BoundType>),
//     // a -> b -> c
//     TypeVar(usize),
// }

// impl BoundType {
//     fn substitute(&mut self, left: usize, right: &BoundType) {
//         match self {
//             BoundType::Named(inner) => {
//                 for ty in inner.type_vars.iter_mut() {
//                     ty.substitute(left, right)
//                 }
//             }
//             BoundType::TypeVar(inner) => {
//                 if *inner == left {
//                     *self = right.clone();
//                 }
//             }
//         }
//     }
//     fn as_named(&self) -> Option<&NamedTy<BoundType>> {
//         if let Self::Named(v) = self {
//             Some(v)
//         } else {
//             None
//         }
//     }
// }

// impl UnboundType {
//     fn substitute(&mut self, left: usize, right: &UnboundType) {
//         match self {
//             UnboundType::Named(inner) => {
//                 for ty in inner.type_vars.iter_mut() {
//                     ty.substitute(left, right)
//                 }
//             }
//             UnboundType::TypeVar(inner) => {
//                 if *inner == left {
//                     *self = right.clone();
//                 }
//             }
//             UnboundType::UnboundNamed(_) => (),
//         }
//     }
//     fn instantiate<'a>(
//         &self,
//         id: &mut GenId,
//         mapping: &mut HashMap<usize, BoundType>,
//     ) -> BoundType {
//         match self {
//             UnboundType::Named(inner) => BoundType::Named(NamedTy {
//                 name: inner.name,
//                 type_vars: inner
//                     .type_vars
//                     .iter()
//                     .map(|type_var| type_var.instantiate(id, mapping))
//                     .collect(),
//             }),
//             UnboundType::UnboundNamed(name) => mapping
//                 .entry(*name)
//                 .or_insert_with(|| id.next_bound_var())
//                 .clone(),
//             UnboundType::TypeVar(v) => BoundType::TypeVar(*v),
//         }
//     }
//     fn bind<'a>(&self, id: &mut GenId, mapping: &mut HashMap<usize, BoundType>) -> UnboundType {
//         match self {
//             UnboundType::Named(inner) => UnboundType::Named(NamedTy {
//                 name: inner.name,
//                 type_vars: inner
//                     .type_vars
//                     .iter()
//                     .map(|type_var| type_var.bind(id, mapping))
//                     .collect(),
//             }),
//             UnboundType::UnboundNamed(name) => mapping
//                 .entry(*name)
//                 .or_insert_with(|| id.next_unbound_var())
//                 .clone(),
//             UnboundType::TypeVar(v) => UnboundType::TypeVar(*v),
//         }
//     }

//     fn as_named(&self) -> Option<&NamedTy<UnboundType>> {
//         if let Self::Named(v) = self {
//             Some(v)
//         } else {
//             None
//         }
//     }
// }

// #[derive(Debug, Clone)]
// enum TypeTree<'a> {
//     Call {
//         name: &'a str,
//         children: Vec<TypeTree<'a>>,
//         ty: UnboundType,
//     },
//     If {
//         condition: Box<TypeTree<'a>>,
//         if_true: Box<TypeTree<'a>>,
//         if_false: Box<TypeTree<'a>>,
//         ty: UnboundType,
//     },
//     Array {
//         children: Vec<TypeTree<'a>>,
//         ty: UnboundType,
//     },
//     Binding {
//         name: &'a str,
//         ty: UnboundType,
//     },
//     Let {
//         names: Vec<(&'a str, TypeTree<'a>)>,
//         expr: Box<TypeTree<'a>>,
//         ty: UnboundType,
//     },
//     Fn {
//         names: Vec<(&'a str, UnboundType)>,
//         eval: Box<TypeTree<'a>>,
//         ty: UnboundType,
//     },
//     Leaf(UnboundType),
// }

// impl<'a> TypeTree<'a> {
//     pub fn ty(&self) -> &UnboundType {
//         match self {
//             TypeTree::Call { ty, .. }
//             | TypeTree::If { ty, .. }
//             | TypeTree::Array { ty, .. }
//             | TypeTree::Binding { ty, .. }
//             | TypeTree::Fn { ty, .. }
//             | TypeTree::Let { ty, .. }
//             | TypeTree::Leaf(ty) => ty,
//         }
//     }
//     pub fn ty_mut(&mut self) -> &mut UnboundType {
//         match self {
//             TypeTree::Call { ty, .. }
//             | TypeTree::If { ty, .. }
//             | TypeTree::Array { ty, .. }
//             | TypeTree::Binding { ty, .. }
//             | TypeTree::Fn { ty, .. }
//             | TypeTree::Let { ty, .. }
//             | TypeTree::Leaf(ty) => ty,
//         }
//     }
// }

// #[derive(Debug, PartialEq, Eq, Default)]
// struct GenId(usize, usize);
// impl GenId {
//     fn next_bound_var(&mut self) -> BoundType {
//         let ret = self.0;
//         self.0 += 1;
//         BoundType::TypeVar(ret)
//     }
//     fn next_unbound_var(&mut self) -> UnboundType {
//         let ret = self.0;
//         self.0 += 1;
//         UnboundType::TypeVar(ret)
//     }
//     fn next_unbound(&mut self) -> UnboundType {
//         let ret = self.1;
//         self.1 += 1;
//         UnboundType::UnboundNamed(ret)
//     }
// }

// #[derive(Debug)]
// struct Constraint<'a> {
//     from: TypeTree<'a>,
//     left: BoundType,
//     right: BoundType,
// }

// fn constraints<'b, 'a>(
//     types: &'b TypeTree<'a>,
//     id: &mut GenId,
//     mut env: HashMap<&'a str, TypeTree<'a>>,
//     mut bindings: HashMap<usize, BoundType>,
// ) -> Vec<Constraint<'a>> {
//     match types {
//         // (+ 1 2)
//         // +: (unbound(1)) (unbound(2)) i32
//         // c:
//         //  (x y i32) <- (a b z)
//         //  (a i32)
//         //  (b i32)
//         TypeTree::Call { name, children, ty } => {
//             let (ret, right, additional) = {
//                 let mut bindings = HashMap::new();
//                 let right = env[*name].ty().instantiate(id, &mut bindings);
//                 let ret = if let TypeTree::Fn { names, ty, .. } = env[*name].clone() {
//                     for (name, ty) in names {
//                         env.insert(
//                             name,
//                             TypeTree::Leaf(ty.instantiate(id, &mut bindings).clone()),
//                         );
//                     }
//                     ty.as_named()
//                         .unwrap()
//                         .type_vars
//                         .last()
//                         .unwrap()
//                         .instantiate(id, &mut bindings)
//                 } else {
//                     panic!()
//                 };
//                 let eval = if let TypeTree::Fn { names, eval, ty } = &env[*name] {
//                     constraints(&eval, id, env.clone(), bindings.clone())
//                 } else {
//                     panic!()
//                 };

//                 (ret, right, eval)
//             };
//             let fn_constraint = Constraint {
//                 from: types.clone(),
//                 left: BoundType::Named(NamedTy {
//                     name: MonoType::Fn,
//                     type_vars: children
//                         .iter()
//                         .map(|x| x.ty().instantiate(id, &mut bindings))
//                         .chain(once(ty.instantiate(id, &mut bindings)))
//                         .collect(),
//                 }),
//                 right,
//             };
//             let eval_constraint = Constraint {
//                 from: types.clone(),
//                 left: ty.instantiate(id, &mut bindings),
//                 right: ret,
//             };
//             children
//                 .iter()
//                 .map(|child| constraints(child, id, env.clone(), bindings.clone()).into_iter())
//                 .flatten()
//                 .chain(additional)
//                 .chain(vec![fn_constraint, eval_constraint])
//                 .collect()
//         }
//         TypeTree::If {
//             condition,
//             if_true,
//             if_false,
//             ty,
//         } => vec![
//             constraints(condition, id, env.clone(), bindings.clone()),
//             constraints(if_true, id, env.clone(), bindings.clone()),
//             constraints(if_false, id, env.clone(), bindings.clone()),
//             vec![
//                 Constraint {
//                     from: types.clone(),
//                     left: condition.ty().instantiate(id, &mut bindings),
//                     right: Type::Named(NamedTy {
//                         name: MonoType::Bool,
//                         type_vars: vec![],
//                     }),
//                 },
//                 Constraint {
//                     from: types.clone(),
//                     left: if_false.ty().instantiate(id, &mut bindings),
//                     right: ty.instantiate(id, &mut bindings),
//                 },
//                 Constraint {
//                     from: types.clone(),
//                     left: if_true.ty().instantiate(id, &mut bindings),
//                     right: ty.instantiate(id, &mut bindings),
//                 },
//             ],
//         ]
//         .into_iter()
//         .flatten()
//         .collect::<Vec<_>>(),
//         TypeTree::Array { children, ty } => {
//             let inner = &ty.as_named().unwrap().type_vars[0];

//             children
//                 .iter()
//                 .map(|lhs| {
//                     //
//                     let mut child_constraints = constraints(lhs, id, env.clone(), bindings.clone());
//                     child_constraints.push(Constraint {
//                         from: types.clone(),
//                         left: inner.instantiate(id, &mut bindings),
//                         right: lhs.ty().instantiate(id, &mut bindings),
//                     });

//                     child_constraints
//                 })
//                 .flatten()
//                 .collect()
//         }
//         TypeTree::Binding { name, ty } => vec![Constraint {
//             from: types.clone(),
//             left: env[*name].ty().instantiate(id, &mut bindings),
//             right: ty.instantiate(id, &mut bindings),
//         }],
//         TypeTree::Fn { eval, ty, names } => {
//             vec![]
//         }
//         TypeTree::Leaf(_) => vec![],
//         TypeTree::Let { names, expr, ty } => {
//             let mut result = vec![Constraint {
//                 from: types.clone(),
//                 left: ty.instantiate(id, &mut bindings),
//                 right: expr.ty().instantiate(id, &mut bindings),
//             }];
//             for (name, ty) in names {
//                 let mut bindings = bindings.clone();
//                 ty.ty().instantiate(id, &mut bindings);
//                 result.extend(constraints(&ty, id, env.clone(), bindings));
//                 env.insert(*name, ty.clone());
//             }
//             result.extend(constraints(expr, id, env.clone(), bindings));

//             result
//         }
//     }
//     //
// }

// #[derive(Clone, Debug, PartialEq, Eq)]
// struct Susbtitution {
//     name: usize,
//     value: Type,
// }

// fn get_substitutions(mut constraints: Vec<Constraint>) -> Result<Vec<Susbtitution>, &'static str> {
//     let mut result: Vec<Susbtitution> = vec![];
//     while !constraints.is_empty() {
//         println!("---- constraints");
//         for c in constraints.iter() {
//             println!("{:?} <= {:?}", c.left, c.right);
//         }
//         let c = constraints.pop().unwrap();
//         match (c.left, c.right) {
//             (Type::Named(left), Type::Named(right)) => {
//                 if left.name != right.name {
//                     return Err("wrong monotype");
//                 } else if left.type_vars.len() != right.type_vars.len() {
//                     return Err("mismatched type_vars");
//                 }
//                 for (left, right) in left.type_vars.iter().zip(right.type_vars.iter()) {
//                     constraints.push(Constraint {
//                         from: c.from.clone(),
//                         left: left.clone(),
//                         right: right.clone(),
//                     });
//                 }
//             }
//             (Type::TypeVar(left), any) | (any, Type::TypeVar(left)) => {
//                 for c in constraints.iter_mut() {
//                     c.left.substitute(left, &any);
//                     c.right.substitute(left, &any);
//                 }
//                 for s in result.iter_mut() {
//                     s.value.substitute(left, &any);
//                 }

//                 result.push(Susbtitution {
//                     name: left,
//                     value: any,
//                 });
//             }
//             (Type::Unbound, _)
//             | (Type::UnboundNamed(_), _)
//             | (_, Type::Unbound)
//             | (_, Type::UnboundNamed(_)) => return Err("error unbound variable"),
//         }
//     }
//     Ok(result)
// }

// fn apply_substitution(tt: &mut TypeTree, substitute: &Susbtitution) {
//     match tt {
//         TypeTree::Call { name, children, ty } => {
//             for child in children.iter_mut() {
//                 apply_substitution(child, substitute);
//             }
//             ty.substitute(substitute.name, &substitute.value)
//         }
//         TypeTree::If {
//             condition,
//             if_true,
//             if_false,
//             ty,
//         } => {
//             apply_substitution(condition, substitute);
//             apply_substitution(if_true, substitute);
//             apply_substitution(if_false, substitute);
//             ty.substitute(substitute.name, &substitute.value)
//         }
//         TypeTree::Array { children, ty } => {
//             for child in children.iter_mut() {
//                 apply_substitution(child, substitute);
//             }
//             ty.substitute(substitute.name, &substitute.value)
//         }
//         TypeTree::Binding { ty, .. } => ty.substitute(substitute.name, &substitute.value),
//         TypeTree::Fn { eval, ty, names } => {
//             for (_, tt) in names.iter_mut() {
//                 tt.substitute(substitute.name, &substitute.value);
//             }
//             apply_substitution(eval, substitute);
//             ty.substitute(substitute.name, &substitute.value);
//         }
//         TypeTree::Leaf(ty) => ty.substitute(substitute.name, &substitute.value),
//         TypeTree::Let { names, expr, ty } => {
//             for (_, tt) in names.iter_mut() {
//                 apply_substitution(tt, substitute);
//             }
//             apply_substitution(expr, substitute);
//             ty.substitute(substitute.name, &substitute.value);
//         }
//     }
// }

// #[test]
// fn ty_check_test() {
//     let code = "(let
//         ((eat (fn (left right) (if left right right))))
//         (eat true 1))";
//     let parsed = crate::parsing::parse_sexpr_value(code).unwrap().1;
//     let bindings = maplit::hashmap! {
//         "print" => TypeTree::Leaf(Type::Named(NamedTy {
//             name: MonoType::Fn,
//             type_vars: vec![Type::UnboundNamed(0), Type::UnboundNamed(0)]
//         }))
//     };
//     let mut id = GenId::default();
//     let mut tt = build_type_tree(parsed, &mut id);
//     let constraints = constraints(&tt, &mut id, bindings, HashMap::new());

//     println!("---- TT");
//     println!("{:?}", tt);
//     let substs = get_substitutions(constraints).unwrap();
//     println!("---- subs");
//     for s in substs {
//         println!("{:?} <- {:?}", s.name, s.value);
//         apply_substitution(&mut tt, &s);
//     }
//     println!("---- TT");
//     println!("{:?}", tt);
//     panic!()
//     //
// }

// fn build_type_tree<'a>(ast: SexprValue<'a>, id: &mut GenId) -> TypeTree<'a> {
//     match ast {
//         SexprValue::Sexpr(inner) => TypeTree::Call {
//             name: inner.target,
//             ty: id.next(),
//             children: inner
//                 .arguments
//                 .into_iter()
//                 .map(|inner| build_type_tree(inner, id))
//                 .collect(),
//         },
//         SexprValue::Symbol(name) => TypeTree::Binding {
//             ty: id.next_unbound(),
//             name,
//         },
//         SexprValue::Integer(_) => TypeTree::Leaf(Type::Named(NamedTy {
//             name: MonoType::Integer,
//             type_vars: vec![],
//         })),
//         SexprValue::Bool(_) => TypeTree::Leaf(Type::Named(NamedTy {
//             name: MonoType::Bool,
//             type_vars: vec![],
//         })),
//         SexprValue::Zone(_) => TypeTree::Leaf(Type::Named(NamedTy {
//             name: MonoType::Zone,
//             type_vars: vec![],
//         })),
//         SexprValue::Unit(_) => TypeTree::Leaf(Type::Named(NamedTy {
//             name: MonoType::Unit,
//             type_vars: vec![],
//         })),
//         SexprValue::None => TypeTree::Leaf(Type::Named(NamedTy {
//             name: MonoType::Option,
//             type_vars: vec![id.next()],
//         })),
//         SexprValue::Array(inner) => TypeTree::Array {
//             children: inner
//                 .into_iter()
//                 .map(|inner| build_type_tree(inner, id))
//                 .collect(),
//             ty: Type::Named(NamedTy {
//                 name: MonoType::Array,
//                 type_vars: vec![id.next()],
//             }),
//         },
//         SexprValue::Fn {
//             arguments, eval, ..
//         } => {
//             let names = arguments
//                 .into_iter()
//                 .map(|(name, _)| (name, id.next_unbound()))
//                 .collect::<Vec<_>>();
//             TypeTree::Fn {
//                 eval: Box::new(build_type_tree(*eval, id)),
//                 ty: Type::Named(NamedTy {
//                     name: MonoType::Fn,
//                     type_vars: names
//                         .iter()
//                         .map(|(_, ty)| ty)
//                         .cloned()
//                         .chain(once(id.next_unbound()))
//                         .collect(),
//                 }),
//                 names,
//             }
//         }
//         SexprValue::If {
//             condition,
//             if_true,
//             if_false,
//         } => TypeTree::If {
//             condition: Box::new(build_type_tree(*condition, id)),
//             if_true: Box::new(build_type_tree(*if_true, id)),
//             if_false: Box::new(build_type_tree(*if_false, id)),
//             ty: id.next(),
//         },
//         SexprValue::Let { bindings, expr } => TypeTree::Let {
//             names: bindings
//                 .into_iter()
//                 .map(|(name, value)| (name, build_type_tree(value, id)))
//                 .collect(),
//             expr: Box::new(build_type_tree(*expr, id)),
//             ty: id.next(),
//         },
//     }
// }

// pub fn type_check<'a>(
//     ast: &'a SexprValue,
//     types: TypeCheckTable<'a>,
//     _executor: &Executor,
// ) -> Result<ValueType, CompileError<'a>> {
//     match ast {
//         SexprValue::Sexpr(Sexpr {
//             target: "print",
//             arguments,
//             ..
//         }) => type_check(&arguments[0], types, _executor),

//         SexprValue::Sexpr(Sexpr {
//             target: "some",
//             arguments,
//             ..
//         }) => {
//             let inner = type_check(&arguments[0], types, _executor)?;
//             if inner == ValueType::Unit {
//                 Ok(ValueType::Unit)
//             } else {
//                 Ok(ValueType::Option(Box::new(inner)))
//             }
//         }
//         SexprValue::Sexpr(sexpr) => {
//             let type_info = types
//                 .fn_types
//                 .get(sexpr.target)
//                 .or_else(|| {
//                     types
//                         .binding_types
//                         .get(sexpr.target)
//                         .and_then(|v| v.as_fn())
//                 })
//                 .ok_or_else(|| CompileError::InvalidFnType {
//                     target: sexpr.target,
//                     found: types.binding_types[sexpr.target].clone(),
//                 })?;
//             for (expected, expr) in type_info
//                 .argument_types
//                 .iter()
//                 .cloned()
//                 .zip(sexpr.arguments.iter())
//             {
//                 let found = type_check(expr, types.clone(), _executor)?;
//                 if !expected.matches(&found) {
//                     return Err(CompileError::InvalidType {
//                         target: sexpr.target,
//                         expected,
//                         found,
//                         value: expr,
//                     });
//                 }
//             }
//             Ok(type_info.return_type.clone())
//         }
//         SexprValue::Symbol(binding, ..) => Ok(types.binding_types[*binding].clone()),
//         SexprValue::Integer(_) => Ok(ValueType::Integer),
//         SexprValue::Zone(_) => Ok(ValueType::Zone),
//         SexprValue::Unit(_) => Ok(ValueType::Unit),
//         SexprValue::Bool(_) => Ok(ValueType::Bool),
//         SexprValue::None => Ok(ValueType::Option(Box::new(ValueType::Unit))),
//         array @ SexprValue::Array(values) => {
//             let types = values
//                 .iter()
//                 .map(|inner| type_check(inner, types.clone(), _executor))
//                 .collect::<Result<Vec<_>, _>>()?;

//             let ret = types.first().unwrap().clone();

//             for ty in types {
//                 if !ty.matches(&ret) {
//                     return Err(CompileError::InvalidType {
//                         target: "array value",
//                         expected: ty,
//                         found: ret,
//                         value: array,
//                     });
//                 }
//             }

//             Ok(ValueType::Array(Box::new(ret)))
//         }
//         SexprValue::Fn {
//             arguments,
//             return_type,
//             eval,
//             ..
//         } => {
//             let mut types = types;
//             for (binding, ty) in arguments.iter().cloned() {
//                 types.binding_types.insert(binding, ty);
//             }
//             let ty = type_check(eval, types, _executor)?;
//             if !ty.matches(return_type) {
//                 Err(CompileError::InvalidType {
//                     target: "fn return type",
//                     expected: return_type.clone(),
//                     found: ty,
//                     value: eval,
//                 })
//             } else {
//                 Ok(ValueType::Fn(Box::new(FnTypeInfo {
//                     argument_types: arguments.iter().map(|(_, ty)| ty).cloned().collect(),
//                     return_type: return_type.clone(),
//                 })))
//             }
//         }
//         SexprValue::Let { bindings, expr, .. } => {
//             let mut types = types;

//             for (key, value) in bindings.iter() {
//                 types
//                     .binding_types
//                     .insert(*key, type_check(value, types.clone(), _executor)?);
//             }
//             //

//             type_check(expr, types, _executor)
//         }
//         SexprValue::If {
//             condition,
//             if_true,
//             if_false,
//             ..
//         } => {
//             let condition_type = type_check(condition, types.clone(), _executor)?;
//             if condition_type != ValueType::Bool {
//                 return Err(CompileError::InvalidCondition {
//                     found: condition_type,
//                     value: condition,
//                 });
//             }

//             let true_type = type_check(if_true, types.clone(), _executor)?;
//             let false_type = type_check(if_false, types, _executor)?;
//             if !true_type.matches(&false_type) {
//                 Err(CompileError::IncompatibleTypes {
//                     true_type,
//                     false_type,
//                     value: if_false,
//                 })
//             } else {
//                 Ok(true_type)
//             }
//         }
//     }
// }
