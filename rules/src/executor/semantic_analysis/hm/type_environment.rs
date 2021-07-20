use std::collections::{BTreeMap, BTreeSet};

use crate::executor::semantic_analysis::hm::{
    type_schemes::TypeScheme,
    types::{Type, TypeVariable},
};

#[derive(Default, Debug, Clone)]
pub(crate) struct TypeEnvironment<'a> {
    pub(crate) map: BTreeMap<&'a str, TypeScheme<'a>>,
}

impl<'a> TypeEnvironment<'a> {
    pub(crate) fn free_variables(&self) -> BTreeSet<TypeVariable> {
        self.map
            .values()
            .flat_map(|scheme| scheme.free_variables())
            .collect()
    }

    pub(crate) fn generalize(&self, ty: Type<'a>) -> TypeScheme<'a> {
        TypeScheme {
            quantified_variables: ty
                .free_variables()
                .difference(&self.free_variables())
                .cloned()
                .collect(),
            ty,
        }
    }
}
