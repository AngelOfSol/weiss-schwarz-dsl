use std::collections::HashSet;

use crate::executor::semantic_analysis::hm::{
    substitution::Substitution,
    types::{Type, TypeVariable},
    Fresh,
};

#[derive(Clone, Debug)]
pub(crate) struct TypeScheme<'a> {
    pub ty: Type<'a>,
    pub type_variables: HashSet<TypeVariable>,
}
impl<'a> TypeScheme<'a> {
    pub(crate) fn free_variables(&self) -> HashSet<TypeVariable> {
        self.ty
            .free_variables()
            .difference(&self.type_variables)
            .copied()
            .collect()
    }

    pub(crate) fn new_vars(&self, fresh: &mut Fresh) -> Type<'a> {
        self.type_variables
            .iter()
            .fold(self.ty.clone(), |acc, elem| {
                acc.instantiate(*elem, fresh.next())
            })
    }
    pub(crate) fn apply(&self, rules: &Substitution<'a>) -> Self {
        let mut rules = rules.clone();
        rules
            .map
            .retain(|var, _| self.free_variables().contains(var));

        Self {
            ty: self.ty.apply(&rules),
            type_variables: self.type_variables.clone(),
        }
    }

    pub(crate) fn generalize_all(ty: Type<'a>) -> Self {
        Self {
            type_variables: ty.free_variables(),
            ty,
        }
    }
}
