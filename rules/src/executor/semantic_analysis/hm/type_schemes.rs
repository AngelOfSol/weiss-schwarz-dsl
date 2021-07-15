use std::collections::{HashMap, HashSet};

use crate::{
    executor::semantic_analysis::hm::{
        substitution::Substitution,
        types::{Type, TypeVariable},
        Fresh,
    },
    parsing::Span,
};

#[derive(Clone, Debug)]
pub struct TypeScheme<'a> {
    pub ty: Type<'a>,
    pub type_variables: HashSet<TypeVariable>,
}
impl<'a> TypeScheme<'a> {
    pub fn free_variables(&self) -> HashSet<TypeVariable> {
        self.ty
            .free_variables()
            .difference(&self.type_variables)
            .copied()
            .collect()
    }
    pub fn apply(&self, rules: &Substitution<'a>) -> Self {
        let mut rules = rules.clone();
        rules
            .map
            .retain(|var, _| self.free_variables().contains(var));

        Self {
            ty: self.ty.apply(&rules),
            type_variables: self.type_variables.clone(),
        }
    }

    pub fn new_vars(&self, fresh: &mut Fresh) -> Type<'a> {
        self.type_variables
            .iter()
            .fold(self.ty.clone(), |acc, elem| {
                acc.instantiate(*elem, fresh.next())
            })
    }
    pub fn instantiate(&self, variables: HashMap<TypeVariable, TypeVariable>) -> Type {
        self.type_variables
            .iter()
            .fold(self.ty.clone(), |acc, elem| {
                acc.instantiate(*elem, variables[elem])
            })
    }

    pub fn generalize_all(ty: Type<'a>) -> Self {
        Self {
            type_variables: ty.free_variables(),
            ty,
        }
    }
}
