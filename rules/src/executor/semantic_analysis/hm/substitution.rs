use std::fmt::Display;

use crate::executor::semantic_analysis::hm::types::{Type, TypeVariable};

#[derive(Clone, Default, Debug)]
pub(crate) struct Substitution<'a> {
    pub map: Vec<(TypeVariable, Type<'a>)>,
}

impl<'a> Substitution<'a> {
    pub(crate) fn union(self, rhs: Self) -> Self {
        for (tv, lhs) in rhs.map.iter() {
            if let Some((og_tv, rhs)) = self.map.iter().find(|(og_tv, _)| og_tv == tv) {
                log::warn!("{}: {} replacing {}: {}", tv, lhs, og_tv, rhs);
            }
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
