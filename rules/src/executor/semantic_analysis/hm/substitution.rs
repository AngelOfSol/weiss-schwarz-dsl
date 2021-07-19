use std::fmt::Display;

use crate::executor::semantic_analysis::hm::types::{Type, TypeVariable};

#[derive(Clone, Default, Debug)]
pub struct Substitution<'a> {
    pub map: Vec<(TypeVariable, Type<'a>)>,
}

impl<'a> Substitution<'a> {
    pub(crate) fn union(self, mut rhs: Self) -> Self {
        for (_, ty) in rhs.map.iter_mut() {
            *ty = ty.apply(&self);
        }

        Self {
            map: self.map.into_iter().chain(rhs.map).collect(),
        }
    }
}

impl<'a> Display for Substitution<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (var, ty) in self.map.iter() {
            writeln!(f, "{}: {}", var, ty)?;
        }
        Ok(())
    }
}
