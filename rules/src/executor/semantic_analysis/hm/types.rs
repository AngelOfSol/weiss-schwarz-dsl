use std::collections::HashSet;

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
    Fn,
    Unit,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Constant {
        name: TypeName,
        parameters: Vec<Type>,
    },
    Var(TypeVariable),
}

impl Type {
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
