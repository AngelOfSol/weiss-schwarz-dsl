use std::{collections::BTreeSet, fmt::Display};

use crate::executor::semantic_analysis::hm::{
    substitution::Substitution,
    types::{Type, TypeVariable},
    Fresh,
};

#[derive(Clone, Debug)]
pub(crate) struct TypeScheme<'a> {
    pub ty: Type<'a>,
    pub quantified_variables: BTreeSet<TypeVariable>,
}

impl<'a> Display for TypeScheme<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<{}>{}",
            self.quantified_variables
                .iter()
                .map(|inner| format!("{}", inner))
                .intersperse(format!(", "))
                .collect::<String>(),
            self.ty
        )
    }
}

impl<'a> TypeScheme<'a> {
    pub(crate) fn free_variables(&self) -> BTreeSet<TypeVariable> {
        self.ty
            .free_variables()
            .difference(&self.quantified_variables)
            .copied()
            .collect()
    }

    pub(crate) fn new_vars(&self, fresh: &mut Fresh) -> Type<'a> {
        self.quantified_variables
            .iter()
            .fold(self.ty.clone(), |acc, elem| {
                acc.instantiate(*elem, fresh.next_type_variable())
            })
    }

    pub(crate) fn generalize_all(ty: Type<'a>) -> Self {
        Self {
            quantified_variables: ty.free_variables(),
            ty,
        }
    }
    pub(crate) fn apply(&mut self, rules: &Substitution<'a>) {
        let mut rules = rules.clone();
        rules
            .map
            .retain(|(var, _)| self.free_variables().contains(var));

        self.ty = self.ty.apply(&rules);
    }
}
