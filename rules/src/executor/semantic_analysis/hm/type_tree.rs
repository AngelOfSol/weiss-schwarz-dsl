use std::fmt::Display;

use crate::{
    executor::{
        semantic_analysis::hm::{
            substitution::Substitution,
            types::{Type, TypeName},
            Fresh,
        },
        value::Value,
    },
    parsing::{Sexpr, Span},
};

#[derive(Debug, Clone)]
pub enum TypedAst<'a> {
    Eval {
        children: Vec<TypedAst<'a>>,
        span: Span<'a>,
        ty: Type<'a>,
    },

    Let {
        bindings: Vec<(&'a str, TypedAst<'a>)>,
        expr: Box<TypedAst<'a>>,
        span: Span<'a>,
        ty: Type<'a>,
    },
    Fn {
        bindings: Vec<(&'a str, Type<'a>)>,
        return_type: Type<'a>,
        expr: Box<TypedAst<'a>>,
        span: Span<'a>,
        ty: Type<'a>,
    },
    Binding {
        name: &'a str,
        span: Span<'a>,
        ty: Type<'a>,
    },
    Seq {
        sub_expressions: Vec<TypedAst<'a>>,
        span: Span<'a>,
        ty: Type<'a>,
    },

    Array {
        values: Vec<TypedAst<'a>>,
        span: Span<'a>,
        ty: Type<'a>,
    },
    If {
        condition: Box<TypedAst<'a>>,
        if_true: Box<TypedAst<'a>>,
        if_false: Box<TypedAst<'a>>,
        span: Span<'a>,
        ty: Type<'a>,
    },
    Value {
        ty: Type<'a>,
        value: Value,
    },
}
impl<'a> Display for TypedAst<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedAst::Eval { children, ty, .. } => {
                write!(
                    f,
                    "({}): {}",
                    children
                        .iter()
                        .map(ToString::to_string)
                        .intersperse("".to_string())
                        .collect::<String>(),
                    ty
                )
            }
            TypedAst::Let {
                bindings, expr, ty, ..
            } => write!(
                f,
                "(let ({}) {}): {}",
                bindings
                    .iter()
                    .map(|(name, ast)| format!("({} {})", name, ast))
                    .intersperse(" ".to_string())
                    .collect::<String>(),
                expr,
                ty
            ),
            TypedAst::Fn {
                bindings,
                return_type,
                expr,
                ty,
                ..
            } => write!(
                f,
                "(fn ({}) -> {} {}): {}",
                bindings
                    .iter()
                    .map(|(name, ast)| format!("{}: {}", name, ast))
                    .intersperse(" ".to_string())
                    .collect::<String>(),
                return_type,
                expr,
                ty
            ),
            TypedAst::Binding { name, ty, .. } => write!(f, "{}: {}", name, ty),
            TypedAst::Seq {
                sub_expressions,
                ty,
                ..
            } => write!(
                f,
                "(seq {}): {}",
                sub_expressions
                    .iter()
                    .map(ToString::to_string)
                    .intersperse(" ".to_string())
                    .collect::<String>(),
                ty
            ),
            TypedAst::Array { values, ty, .. } => write!(
                f,
                "({}): {}",
                values
                    .iter()
                    .map(ToString::to_string)
                    .intersperse(" ".to_string())
                    .collect::<String>(),
                ty
            ),
            TypedAst::If {
                condition,
                if_true,
                if_false,
                ty,
                ..
            } => write!(f, "(if {} {} {}): {}", condition, if_false, if_true, ty),
            TypedAst::Value { value, .. } => write!(f, "{}", value),
        }
    }
}

impl<'a> TypedAst<'a> {
    pub(crate) fn span(&self) -> &Span<'a> {
        match self {
            TypedAst::Eval { span, .. }
            | TypedAst::Let { span, .. }
            | TypedAst::Fn { span, .. }
            | TypedAst::Binding { span, .. }
            | TypedAst::Array { span, .. }
            | TypedAst::If { span, .. }
            | TypedAst::Seq { span, .. } => span,
            TypedAst::Value { ty, .. } => ty.span(),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn ty(&self) -> &Type<'a> {
        match self {
            TypedAst::Eval { ty, .. }
            | TypedAst::Let { ty, .. }
            | TypedAst::Fn { ty, .. }
            | TypedAst::Binding { ty, .. }
            | TypedAst::Array { ty, .. }
            | TypedAst::If { ty, .. }
            | TypedAst::Seq { ty, .. }
            | TypedAst::Value { ty, .. } => ty,
        }
    }
    pub(crate) fn ty_mut(&mut self) -> &mut Type<'a> {
        match self {
            TypedAst::Eval { ty, .. }
            | TypedAst::Let { ty, .. }
            | TypedAst::Fn { ty, .. }
            | TypedAst::Binding { ty, .. }
            | TypedAst::Array { ty, .. }
            | TypedAst::If { ty, .. }
            | TypedAst::Seq { ty, .. }
            | TypedAst::Value { ty, .. } => ty,
        }
    }
    #[allow(dead_code)]
    pub fn apply(&mut self, rules: &Substitution<'a>) {
        *self.ty_mut() = self.ty_mut().apply(rules);

        match self {
            TypedAst::Eval { children, .. } => {
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

            TypedAst::Binding { ty, .. } => *ty = ty.apply(rules),
            TypedAst::Value { ty, .. } => *ty = ty.apply(rules),
            TypedAst::Fn {
                expr,
                return_type,
                bindings,
                ..
            } => {
                for (_, ty) in bindings {
                    *ty = ty.apply(rules);
                }
                *return_type = return_type.apply(rules);
                expr.apply(rules);
            }
            TypedAst::Seq {
                sub_expressions, ..
            } => {
                for expr in sub_expressions {
                    expr.apply(rules);
                }
            }
            TypedAst::If {
                condition,
                if_true,
                if_false,
                ..
            } => {
                condition.apply(rules);
                if_true.apply(rules);
                if_false.apply(rules);
            }
        }
    }
}

pub(crate) fn build_type_tree<'a>(sexpr: Sexpr<'a>, fresh: &mut Fresh) -> TypedAst<'a> {
    match sexpr {
        Sexpr::Eval { span, arguments } => TypedAst::Eval {
            children: arguments
                .into_iter()
                .map(|arg| build_type_tree(arg, fresh))
                .collect(),
            span: span,
            ty: Type::Var(fresh.next(), span),
        },
        Sexpr::Symbol(binding, span) => TypedAst::Binding {
            name: binding,
            span: span,
            ty: Type::Var(fresh.next(), span),
        },
        Sexpr::Integer(value, span) => TypedAst::Value {
            ty: Type::Constant {
                span: span,
                name: TypeName::Integer,
                parameters: vec![],
            },
            value: Value::from(value),
        },
        Sexpr::Bool(value, span) => TypedAst::Value {
            ty: Type::Constant {
                span: span,
                name: TypeName::Bool,
                parameters: vec![],
            },
            value: Value::from(value),
        },
        Sexpr::Zone(value, span) => TypedAst::Value {
            ty: Type::Constant {
                span: span,
                name: TypeName::Zone,
                parameters: vec![],
            },
            value: Value::from(value),
        },
        Sexpr::Unit(span) => TypedAst::Value {
            ty: Type::Constant {
                span: span,
                name: TypeName::Unit,
                parameters: vec![],
            },
            value: Value::Unit,
        },
        Sexpr::None(span) => TypedAst::Value {
            ty: Type::Constant {
                span: span,
                name: TypeName::Option,
                parameters: vec![Type::Var(fresh.next(), span)],
            },
            value: Value::None,
        },
        Sexpr::Array { span, values } => TypedAst::Array {
            span: span,
            values: values
                .into_iter()
                .map(|value| build_type_tree(value, fresh))
                .collect(),
            ty: Type::Var(fresh.next(), span),
        },
        Sexpr::Fn {
            arguments,
            eval,
            span,
            return_type,
        } => TypedAst::Fn {
            bindings: arguments.clone(),
            return_type: return_type
                .clone()
                .unwrap_or_else(|| Type::Var(fresh.next(), span)),
            expr: Box::new(build_type_tree(*eval, fresh)),
            span: span,
            ty: Type::Var(fresh.next(), span),
        },
        Sexpr::If {
            condition,
            if_true,
            if_false,
            span,
        } => TypedAst::If {
            condition: Box::new(build_type_tree(*condition, fresh)),
            if_true: Box::new(build_type_tree(*if_true, fresh)),
            if_false: Box::new(build_type_tree(*if_false, fresh)),
            span: span,
            ty: Type::Var(fresh.next(), span),
        },
        Sexpr::Let {
            bindings,
            expr,
            span,
        } => TypedAst::Let {
            bindings: bindings
                .into_iter()
                .map(|(key, value)| (key, build_type_tree(value, fresh)))
                .collect(),
            expr: Box::new(build_type_tree(*expr, fresh)),
            span: span,
            ty: Type::Var(fresh.next(), span),
        },
        Sexpr::Seq {
            sub_expressions,
            span,
        } => TypedAst::Seq {
            sub_expressions: sub_expressions
                .into_iter()
                .map(|expr| build_type_tree(expr, fresh))
                .collect(),
            span: span,
            ty: Type::Var(fresh.next(), span),
        },
    }
}
