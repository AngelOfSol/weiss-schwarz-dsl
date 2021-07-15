use std::collections::HashMap;

use crate::executor::semantic_analysis::hm::types::{Type, TypeVariable};

#[derive(Clone, Default, Debug)]
pub struct Substitution {
    pub map: HashMap<TypeVariable, Type>,
}

impl Substitution {
    pub fn union(self, rhs: Self) -> Self {
        Self {
            map: self.map.into_iter().chain(rhs.map).collect(),
        }
    }
}
