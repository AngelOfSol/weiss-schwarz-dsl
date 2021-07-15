use std::collections::HashMap;

use crate::executor::semantic_analysis::hm::types::{Type, TypeVariable};

#[derive(Clone, Default, Debug)]
pub(crate) struct Substitution<'a> {
    pub map: HashMap<TypeVariable, Type<'a>>,
}

impl<'a> Substitution<'a> {
    pub(crate) fn union(self, rhs: Self) -> Self {
        Self {
            map: self.map.into_iter().chain(rhs.map).collect(),
        }
    }
}
