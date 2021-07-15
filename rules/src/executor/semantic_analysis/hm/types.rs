use std::{collections::HashSet, fmt::Display};

use crate::executor::semantic_analysis::hm::substitution::Substitution;

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
pub enum Type {
    Constant {
        name: TypeName,
        parameters: Vec<Type>,
    },
    Var(TypeVariable),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Constant { name, parameters } => match name {
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
                    "{}?",
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
            Type::Var(var) => write!(f, "@{}", var.0),
        }
    }
}

impl Type {
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
            Type::Var(idx) => maplit::hashset! { *idx },
        }
    }

    pub fn instantiate(self, left: TypeVariable, right: TypeVariable) -> Self {
        match self {
            Type::Constant { name, parameters } => Type::Constant {
                name: name,
                parameters: parameters
                    .into_iter()
                    .map(|ty| ty.instantiate(left, right))
                    .collect(),
            },
            Type::Var(v) if left == v => Type::Var(right),
            value => value,
        }
    }

    pub fn apply(&self, rules: &Substitution) -> Self {
        match self {
            Type::Constant { name, parameters } => Type::Constant {
                name: *name,
                parameters: parameters.iter().map(|ty| ty.apply(rules)).collect(),
            },
            Type::Var(v) => rules.map.get(v).cloned().unwrap_or(Type::Var(*v)),
        }
    }
}
