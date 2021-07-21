use std::fmt::Display;

use arcstr::Substr;

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
pub enum TypedAst {
    Eval {
        children: Vec<TypedAst>,
        span: Span,
        ty: Type,
    },

    Let {
        bindings: Vec<(Substr, TypedAst)>,
        expr: Box<TypedAst>,
        span: Span,
        ty: Type,
    },
    Fn {
        bindings: Vec<(Substr, Type)>,
        return_type: Type,
        expr: Box<TypedAst>,
        span: Span,
        ty: Type,
    },
    Binding {
        name: Substr,
        span: Span,
        ty: Type,
    },
    Seq {
        sub_expressions: Vec<TypedAst>,
        span: Span,
        ty: Type,
    },

    Array {
        values: Vec<TypedAst>,
        span: Span,
        ty: Type,
    },
    If {
        condition: Box<TypedAst>,
        if_true: Box<TypedAst>,
        if_false: Box<TypedAst>,
        span: Span,
        ty: Type,
    },
    Value {
        ty: Type,
        value: Value,
    },
}
impl Display for TypedAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedAst::Eval { children, ty, .. } => {
                write!(
                    f,
                    "({}): {}",
                    children
                        .iter()
                        .map(ToString::to_string)
                        .intersperse(" ".to_string())
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

impl TypedAst {
    pub(crate) fn span(&self) -> &Span {
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
    pub(crate) fn ty(&self) -> &Type {
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
    pub(crate) fn ty_mut(&mut self) -> &mut Type {
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
    pub fn apply(&mut self, rules: &Substitution) {
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

pub(crate) fn build_type_tree(sexpr: Sexpr, fresh: &mut Fresh) -> TypedAst {
    match sexpr {
        Sexpr::Eval { span, arguments } => TypedAst::Eval {
            children: arguments
                .into_iter()
                .map(|arg| build_type_tree(arg, fresh))
                .collect(),
            ty: Type::Var(fresh.next_type_variable(), span.clone()),
            span,
        },
        Sexpr::Symbol(binding, span) => TypedAst::Binding {
            name: binding,
            ty: Type::Var(fresh.next_type_variable(), span.clone()),
            span,
        },
        Sexpr::Integer(value, span) => TypedAst::Value {
            ty: Type::Constant {
                span,
                name: TypeName::Integer,
                parameters: vec![],
            },
            value: Value::from(value),
        },
        Sexpr::Bool(value, span) => TypedAst::Value {
            ty: Type::Constant {
                span,
                name: TypeName::Bool,
                parameters: vec![],
            },
            value: Value::from(value),
        },
        Sexpr::Zone(value, span) => TypedAst::Value {
            ty: Type::Constant {
                span,
                name: TypeName::Zone,
                parameters: vec![],
            },
            value: Value::from(value),
        },
        Sexpr::Unit(span) => TypedAst::Value {
            ty: Type::Constant {
                span,
                name: TypeName::Unit,
                parameters: vec![],
            },
            value: Value::Unit,
        },
        Sexpr::None(span) => TypedAst::Value {
            ty: Type::Constant {
                name: TypeName::Option,
                parameters: vec![Type::Var(fresh.next_type_variable(), span.clone())],
                span,
            },
            value: Value::None,
        },
        Sexpr::Array { span, values } => TypedAst::Array {
            values: values
                .into_iter()
                .map(|value| build_type_tree(value, fresh))
                .collect(),
            ty: Type::Var(fresh.next_type_variable(), span.clone()),
            span,
        },
        Sexpr::Fn {
            arguments,
            eval,
            span,
            return_type,
        } => {
            let mut bindings = Default::default();
            TypedAst::Fn {
                bindings: arguments
                    .into_iter()
                    .map(|(name, ty)| (name, ty.remap(&mut bindings, fresh)))
                    .collect(),
                return_type: return_type
                    .clone()
                    .map(|ty| ty.remap(&mut bindings, fresh))
                    .unwrap_or_else(|| Type::Var(fresh.next_type_variable(), span.clone())),
                expr: Box::new(build_type_tree(*eval, fresh)),
                ty: Type::Var(fresh.next_type_variable(), span.clone()),
                span,
            }
        }
        Sexpr::If {
            condition,
            if_true,
            if_false,
            span,
        } => TypedAst::If {
            condition: Box::new(build_type_tree(*condition, fresh)),
            if_true: Box::new(build_type_tree(*if_true, fresh)),
            if_false: Box::new(build_type_tree(*if_false, fresh)),
            ty: Type::Var(fresh.next_type_variable(), span.clone()),
            span,
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
            ty: Type::Var(fresh.next_type_variable(), span.clone()),
            span,
        },
        Sexpr::Seq {
            sub_expressions,
            span,
        } => TypedAst::Seq {
            sub_expressions: sub_expressions
                .into_iter()
                .map(|expr| build_type_tree(expr, fresh))
                .collect(),
            ty: Type::Var(fresh.next_type_variable(), span.clone()),
            span,
        },
    }
}
