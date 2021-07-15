use std::{collections::HashSet, fmt::Display};

use crate::{executor::semantic_analysis::hm::substitution::Substitution, parsing::Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable(usize);

impl TypeVariable {
    pub fn new(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeName {
    Integer,
    Bool,
    Option,
    Array,
    Zone,
    Card,
    Fn,
    Unit,
}

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Integer => write!(f, "i32"),
            TypeName::Bool => write!(f, "bool"),
            TypeName::Option => write!(f, "?"),
            TypeName::Array => write!(f, "[]"),
            TypeName::Zone => write!(f, "zone"),
            TypeName::Card => write!(f, "card"),
            TypeName::Fn => write!(f, "fn"),
            TypeName::Unit => write!(f, "()"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type<'a> {
    Constant {
        name: TypeName,
        parameters: Vec<Type<'a>>,
        span: Span<'a>,
    },
    Var(TypeVariable, Span<'a>),
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Constant {
                name, parameters, ..
            } => match name {
                TypeName::Fn => write!(
                    f,
                    "fn({}) -> {}",
                    parameters
                        .iter()
                        .take(parameters.len() - 1)
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    parameters.last().unwrap(),
                ),
                TypeName::Option => write!(
                    f,
                    "?{}",
                    parameters
                        .iter()
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                TypeName::Array => write!(
                    f,
                    "[{}]",
                    parameters
                        .iter()
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                name => {
                    write!(f, "{}", name)
                }
            },
            Type::Var(var, ..) => write!(f, "@{}", var.0),
        }
    }
}

impl<'a> Type<'a> {
    pub fn is_concrete(&self) -> bool {
        match self {
            Type::Constant { parameters, .. } => parameters.iter().all(|child| child.is_concrete()),
            Type::Var(_, _) => false,
        }
    }

    pub fn occurs(&self, type_var: &TypeVariable) -> bool {
        match self {
            Type::Constant { parameters, .. } => {
                parameters.iter().any(|item| item.occurs(type_var))
            }
            Type::Var(x, ..) => x == type_var,
        }
    }
    pub fn argument_count(&self) -> Option<usize> {
        if let Type::Constant {
            name: TypeName::Fn,
            parameters,
            ..
        } = self
        {
            Some(parameters.len() - 1)
        } else {
            None
        }
    }
    pub fn free_variables(&self) -> HashSet<TypeVariable> {
        match self {
            Type::Constant { parameters, .. } => parameters
                .iter()
                .flat_map(|item| item.free_variables())
                .collect(),
            Type::Var(idx, ..) => maplit::hashset! { *idx },
        }
    }
    pub fn span(&self) -> &Span<'a> {
        match self {
            Type::Constant { span, .. } => span,
            Type::Var(_, span) => span,
        }
    }
    pub fn with_span(self, span: Span<'a>) -> Type<'a> {
        match self {
            Type::Constant {
                name, parameters, ..
            } => Type::Constant {
                name,
                parameters,
                span,
            },
            Type::Var(var, _) => Type::Var(var, span),
        }
    }

    pub(crate) fn instantiate(self, left: TypeVariable, right: TypeVariable) -> Self {
        match self {
            Type::Constant {
                name,
                parameters,
                span,
            } => Type::Constant {
                name: name,
                parameters: parameters
                    .into_iter()
                    .map(|ty| ty.instantiate(left, right))
                    .collect(),
                span,
            },
            Type::Var(v, span) if left == v => Type::Var(right, span),
            value => value,
        }
    }

    pub(crate) fn apply(&self, rules: &Substitution<'a>) -> Self {
        match self {
            Type::Constant {
                name,
                parameters,
                span,
            } => Type::Constant {
                name: *name,
                parameters: parameters.iter().map(|ty| ty.apply(rules)).collect(),
                span: *span,
            },
            Type::Var(v, span) => rules.map.get(v).cloned().unwrap_or(Type::Var(*v, *span)),
        }
    }

    pub fn integer(span: Span<'a>) -> Self {
        Self::Constant {
            name: TypeName::Integer,
            parameters: vec![],
            span,
        }
    }
    pub fn unit(span: Span<'a>) -> Self {
        Self::Constant {
            name: TypeName::Unit,
            parameters: vec![],
            span,
        }
    }
    pub fn boolean(span: Span<'a>) -> Self {
        Self::Constant {
            name: TypeName::Bool,
            parameters: vec![],
            span,
        }
    }
    pub fn zone(span: Span<'a>) -> Self {
        Self::Constant {
            name: TypeName::Zone,
            parameters: vec![],
            span,
        }
    }
    pub fn card(span: Span<'a>) -> Self {
        Self::Constant {
            name: TypeName::Card,
            parameters: vec![],
            span,
        }
    }
    pub fn option(ty: Type<'a>, span: Span<'a>) -> Self {
        Self::Constant {
            name: TypeName::Option,
            parameters: vec![ty],
            span,
        }
    }
    pub fn array(ty: Type<'a>, span: Span<'a>) -> Self {
        Self::Constant {
            name: TypeName::Array,
            parameters: vec![ty],
            span,
        }
    }

    pub fn function(tys: Vec<Type<'a>>, span: Span<'a>) -> Self {
        Self::Constant {
            name: TypeName::Fn,
            parameters: tys,
            span,
        }
    }
    pub fn type_var(ty: TypeVariable, span: Span<'a>) -> Self {
        Self::Var(ty, span)
    }
}
