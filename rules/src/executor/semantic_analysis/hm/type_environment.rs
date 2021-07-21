use std::collections::{BTreeMap, BTreeSet};

use arcstr::Substr;

use crate::executor::semantic_analysis::hm::{
    substitution::Substitution,
    type_schemes::TypeScheme,
    types::{Type, TypeVariable},
};

#[derive(Default, Debug, Clone)]
pub(crate) struct TypeEnvironment {
    pub(crate) map: BTreeMap<Substr, TypeScheme>,
}

impl TypeEnvironment {
    pub(crate) fn free_variables(&self) -> BTreeSet<TypeVariable> {
        self.map
            .values()
            .flat_map(|scheme| scheme.free_variables())
            .collect()
    }

    pub(crate) fn generalize(&self, ty: Type) -> TypeScheme {
        TypeScheme {
            quantified_variables: ty
                .free_variables()
                .difference(&self.free_variables())
                .cloned()
                .collect(),
            ty,
        }
    }
    pub(crate) fn apply(&mut self, rules: &Substitution) {
        for scheme in self.map.values_mut() {
            scheme.apply(rules);
        }
    }
}
