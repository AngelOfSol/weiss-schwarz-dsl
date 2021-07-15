use std::collections::{HashMap, HashSet};

use crate::executor::semantic_analysis::hm::{
    substitution::Substitution,
    types::{Type, TypeVariable},
    Fresh,
};

#[derive(Clone, Debug)]
pub struct TypeScheme {
    pub ty: Type,
    pub type_variables: HashSet<TypeVariable>,
}
impl TypeScheme {
    pub fn free_variables(&self) -> HashSet<TypeVariable> {
        self.ty
            .free_variables()
            .difference(&self.type_variables)
            .copied()
            .collect()
    }
    pub fn apply(&self, rules: &Substitution) -> Self {
        let mut rules = rules.clone();
        rules
            .map
            .retain(|var, _| self.free_variables().contains(var));

        Self {
            ty: self.ty.apply(&rules),
            type_variables: self.type_variables.clone(),
        }
    }

    pub fn new_vars(&self, fresh: &mut Fresh) -> Type {
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
}
