use std::collections::{HashMap, HashSet};

use crate::executor::semantic_analysis::hm::{
    substitution::Substitution,
    type_schemes::TypeScheme,
    types::{Type, TypeVariable},
};

#[derive(Default, Debug, Clone)]
pub struct TypeEnvironment<'a> {
    pub map: HashMap<&'a str, TypeScheme<'a>>,
}

impl<'a> TypeEnvironment<'a> {
    pub fn free_variables(&self) -> HashSet<TypeVariable> {
        self.map
            .values()
            .flat_map(|scheme| scheme.free_variables())
            .collect()
    }

    pub fn apply(&self, rules: &'a Substitution<'a>) -> Self {
        Self {
            map: self
                .map
                .iter()
                .map(|(key, scheme)| (*key, scheme.apply(rules)))
                .collect(),
        }
    }

    pub fn generalize(&self, ty: Type<'a>) -> TypeScheme<'a> {
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
