use crate::{
    executor::semantic_analysis::hm::{
        substitution::Substitution,
        types::{Type, TypeName},
        Fresh,
    },
    parsing::{SexprValue, Span},
};

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

    Array {
        values: Vec<TypeTree<'a>>,
        span: Span<'a>,
    },
    Leaf(Type<'a>),
}

impl<'a> TypeTree<'a> {
    pub(crate) fn span(&self) -> &Span<'a> {
        match self {
            TypeTree::Call { span, .. }
            | TypeTree::Let { span, .. }
            | TypeTree::Fn { span, .. }
            | TypeTree::Binding { span, .. }
            | TypeTree::Array { span, .. }
            | TypeTree::Seq { span, .. } => span,
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
            TypeTree::Array { values, .. } => {
                for value in values {
                    value.apply(rules);
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
        SexprValue::Array { span, values } => TypeTree::Array {
            span: *span,
            values: values
                .iter()
                .map(|value| build_type_tree(value, fresh))
                .collect(),
        },
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
