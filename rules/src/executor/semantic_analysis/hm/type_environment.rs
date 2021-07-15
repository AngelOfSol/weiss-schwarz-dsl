use std::collections::{HashMap, HashSet};

use crate::executor::semantic_analysis::hm::{
    substitution::Substitution,
    type_schemes::TypeScheme,
    types::{Type, TypeVariable},
};

#[derive(Default, Debug)]
pub struct TypeEnvironment<'a> {
    pub map: HashMap<&'a str, TypeScheme>,
}

impl<'a> TypeEnvironment<'a> {
    #[allow(dead_code)]
    pub fn remove(&self, variable: &'a str) -> Self {
        Self {
            map: self
                .map
                .iter()
                .filter(|(k, _)| **k != variable)
                .map(|(k, v)| (*k, v.clone()))
                .collect(),
        }
    }

    pub fn free_variables(&self) -> HashSet<TypeVariable> {
        self.map
            .values()
            .flat_map(|scheme| scheme.free_variables())
            .collect()
    }

    pub fn apply(&self, rules: &Substitution) -> Self {
        Self {
            map: self
                .map
                .iter()
                .map(|(key, scheme)| (*key, scheme.apply(rules)))
                .collect(),
        }
    }

    pub fn generalize(&self, ty: Type) -> TypeScheme {
        TypeScheme {
            type_variables: ty
                .free_variables()
                .difference(&self.free_variables())
                .cloned()
                .collect(),
            ty,
        }
    }
}
