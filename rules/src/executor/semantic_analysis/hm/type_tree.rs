use crate::{
    executor::semantic_analysis::hm::{
        substitution::Substitution,
        types::{Type, TypeName},
        Fresh,
    },
    parsing::{Sexpr, Span},
};

#[derive(Debug, Clone)]
pub(crate) enum TypedAst<'a> {
    Call {
        children: Vec<TypedAst<'a>>,
        span: Span<'a>,
    },

    Let {
        bindings: Vec<(&'a str, TypedAst<'a>)>,
        expr: Box<TypedAst<'a>>,
        span: Span<'a>,
    },
    Fn {
        bindings: Vec<(&'a str, Type<'a>)>,
        return_type: Type<'a>,
        expr: Box<TypedAst<'a>>,
        span: Span<'a>,
    },
    Binding {
        name: &'a str,
        span: Span<'a>,
    },
    Seq {
        sub_expressions: Vec<TypedAst<'a>>,
        span: Span<'a>,
    },

    Array {
        values: Vec<TypedAst<'a>>,
        span: Span<'a>,
    },
    Leaf(Type<'a>),
}

impl<'a> TypedAst<'a> {
    pub(crate) fn span(&self) -> &Span<'a> {
        match self {
            TypedAst::Call { span, .. }
            | TypedAst::Let { span, .. }
            | TypedAst::Fn { span, .. }
            | TypedAst::Binding { span, .. }
            | TypedAst::Array { span, .. }
            | TypedAst::Seq { span, .. } => span,
            TypedAst::Leaf(inner) => inner.span(),
        }
    }
    #[allow(dead_code)]
    fn apply(&mut self, rules: &Substitution<'a>) {
        match self {
            TypedAst::Call { children, .. } => {
                for child in children {
                    child.apply(rules);
                }
            }
            TypedAst::Array { values, .. } => {
                for value in values {
                    value.apply(rules);
                }
            }
            TypedAst::Let { bindings, expr, .. } => {
                for (_, ty) in bindings {
                    ty.apply(rules);
                }
                expr.apply(rules);
            }

            TypedAst::Binding { .. } => (),
            TypedAst::Leaf(ty) => *ty = ty.apply(rules),
            TypedAst::Fn { expr, .. } => {
                expr.apply(rules);
            }
            TypedAst::Seq {
                sub_expressions, ..
            } => {
                for expr in sub_expressions {
                    expr.apply(rules);
                }
            }
        }
    }
}

pub(crate) fn build_type_tree<'a>(sexpr: &Sexpr<'a>, fresh: &mut Fresh) -> TypedAst<'a> {
    match sexpr {
        Sexpr::Eval {
            target,
            span,
            arguments,
        } => TypedAst::Call {
            children: vec![TypedAst::Binding {
                name: target,
                span: *span,
            }]
            .into_iter()
            .chain(arguments.iter().map(|arg| build_type_tree(arg, fresh)))
            .collect(),
            span: *span,
        },
        Sexpr::Symbol(binding, span) => TypedAst::Binding {
            name: *binding,
            span: *span,
        },
        Sexpr::Integer(_, span) => TypedAst::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Integer,
            parameters: vec![],
        }),
        Sexpr::Bool(_, span) => TypedAst::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Bool,
            parameters: vec![],
        }),
        Sexpr::Zone(_, span) => TypedAst::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Zone,
            parameters: vec![],
        }),
        Sexpr::Unit(span) => TypedAst::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Unit,
            parameters: vec![],
        }),
        Sexpr::None(span) => TypedAst::Leaf(Type::Constant {
            span: *span,
            name: TypeName::Option,
            parameters: vec![Type::Var(fresh.next(), *span)],
        }),
        Sexpr::Array { span, values } => TypedAst::Array {
            span: *span,
            values: values
                .iter()
                .map(|value| build_type_tree(value, fresh))
                .collect(),
        },
        Sexpr::Fn {
            arguments,
            eval,
            span,
            return_type,
            ..
        } => TypedAst::Fn {
            bindings: arguments.clone(),
            return_type: return_type.clone(),
            expr: Box::new(build_type_tree(eval, fresh)),
            span: *span,
        },
        Sexpr::If {
            condition,
            if_true,
            if_false,
            span,
            ..
        } => TypedAst::Call {
            children: vec![
                TypedAst::Binding {
                    name: "if",
                    span: *span,
                },
                build_type_tree(condition, fresh),
                build_type_tree(if_true, fresh),
                build_type_tree(if_false, fresh),
            ],
            span: *span,
        },
        Sexpr::Let {
            bindings,
            expr,
            span,
            ..
        } => TypedAst::Let {
            bindings: bindings
                .iter()
                .map(|(key, value)| (*key, build_type_tree(value, fresh)))
                .collect(),
            expr: Box::new(build_type_tree(expr, fresh)),
            span: *span,
        },
        Sexpr::Seq {
            sub_expressions,
            span,
        } => TypedAst::Seq {
            sub_expressions: sub_expressions
                .iter()
                .map(|expr| build_type_tree(expr, fresh))
                .collect(),
            span: *span,
        },
    }
}
