use crate::{executor::semantic_analysis::hm::substitution::Substitution, parsing::Span};
use derivative::Derivative;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter::FromIterator,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable(usize);

impl TypeVariable {
    pub fn new(idx: usize) -> Self {
        Self(idx)
    }
}

impl Display for TypeVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeName {
    Integer,
    Bool,
    Option,
    Array,
    Zone,
    Card,
    Fn,
    Unit,
}

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Integer => write!(f, "i32"),
            TypeName::Bool => write!(f, "bool"),
            TypeName::Option => write!(f, "?"),
            TypeName::Array => write!(f, "[]"),
            TypeName::Zone => write!(f, "zone"),
            TypeName::Card => write!(f, "card"),
            TypeName::Fn => write!(f, "fn"),
            TypeName::Unit => write!(f, "()"),
        }
    }
}
pub type Type = GenericType<TypeVariable>;

#[derive(Derivative)]
#[derivative(
    PartialEq,
    Eq,
    PartialOrd = "feature_allow_slow_enum",
    Ord = "feature_allow_slow_enum"
)]
#[derive(Clone, Debug)]
pub enum GenericType<TV = TypeVariable> {
    Constant {
        name: TypeName,
        parameters: Vec<GenericType<TV>>,
        #[derivative(PartialEq = "ignore")]
        #[derivative(PartialOrd = "ignore")]
        #[derivative(Ord = "ignore")]
        span: Span,
    },
    Var(
        TV,
        #[derivative(PartialEq = "ignore")]
        #[derivative(PartialOrd = "ignore")]
        #[derivative(Ord = "ignore")]
        Span,
    ),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Constant {
                name, parameters, ..
            } => match name {
                TypeName::Fn => write!(
                    f,
                    "fn({}) -> {}",
                    parameters
                        .iter()
                        .take(parameters.len() - 1)
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    parameters.last().unwrap(),
                ),
                TypeName::Option => write!(
                    f,
                    "?{}",
                    parameters
                        .iter()
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                TypeName::Array => write!(
                    f,
                    "[{}]",
                    parameters
                        .iter()
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                name => {
                    write!(f, "{}", name)
                }
            },
            Type::Var(var, ..) => write!(f, "{}", var),
        }
    }
}

impl Type {
    pub fn is_concrete(&self) -> bool {
        match self {
            Type::Constant { parameters, .. } => parameters.iter().all(|child| child.is_concrete()),
            Type::Var(_, _) => false,
        }
    }

    pub fn occurs(&self, type_var: &TypeVariable) -> bool {
        match self {
            Type::Constant { parameters, .. } => {
                parameters.iter().any(|item| item.occurs(type_var))
            }
            Type::Var(x, ..) => x == type_var,
        }
    }
    pub fn argument_count(&self) -> Option<usize> {
        if let Type::Constant {
            name: TypeName::Fn,
            parameters,
            ..
        } = self
        {
            Some(parameters.len() - 1)
        } else {
            None
        }
    }
    pub fn free_variables(&self) -> HashSet<TypeVariable> {
        match self {
            Type::Constant { parameters, .. } => parameters
                .iter()
                .flat_map(|item| item.free_variables())
                .collect(),
            Type::Var(idx, ..) => maplit::hashset! { *idx },
        }
    }
    pub fn span(&self) -> &Span {
        match self {
            Type::Constant { span, .. } => span,
            Type::Var(_, span) => span,
        }
    }
    pub fn with_span(self, span: Span) -> Type {
        match self {
            Type::Constant {
                name, parameters, ..
            } => Type::Constant {
                name,
                parameters,
                span,
            },
            Type::Var(var, _) => Type::Var(var, span),
        }
    }

    pub fn remap<I: Iterator>(
        self,
        bindings: &mut HashMap<TypeVariable, I::Item>,
        fresh: &mut I,
    ) -> GenericType<I::Item>
    where
        Vec<GenericType>: FromIterator<GenericType<<I as Iterator>::Item>>,
        I::Item: Clone,
    {
        match self {
            Type::Constant {
                parameters,
                name,
                span,
            } => GenericType::Constant {
                parameters: parameters
                    .into_iter()
                    .map(|x| x.remap(bindings, fresh))
                    .collect(),
                name,
                span,
            },
            Type::Var(v, span) => {
                let entry = bindings.entry(v).or_insert_with(|| fresh.next().unwrap());
                GenericType::type_var(entry.clone(), span)
            }
        }
    }

    pub(crate) fn instantiate(self, left: TypeVariable, right: TypeVariable) -> Self {
        match self {
            Type::Constant {
                name,
                parameters,
                span,
            } => Type::Constant {
                name: name,
                parameters: parameters
                    .into_iter()
                    .map(|ty| ty.instantiate(left, right))
                    .collect(),
                span,
            },
            Type::Var(v, span) if left == v => Type::Var(right, span),
            value => value,
        }
    }

    fn apply_rule(&mut self, left: &TypeVariable, ty: &Type) {
        match self {
            Type::Constant { parameters, .. } => {
                for parameter in parameters {
                    parameter.apply_rule(left, ty);
                }
            }
            Type::Var(v, span) => {
                if v == left {
                    *self = ty.clone().with_span(span.clone());
                }
            }
        }
    }
    pub(crate) fn apply(&self, rules: &Substitution) -> Self {
        let mut ret = self.clone();
        for (left, ty) in rules.map.iter() {
            ret.apply_rule(left, ty)
        }
        ret
    }

    pub fn integer(span: Span) -> Self {
        Self::Constant {
            name: TypeName::Integer,
            parameters: vec![],
            span,
        }
    }
    pub fn unit(span: Span) -> Self {
        Self::Constant {
            name: TypeName::Unit,
            parameters: vec![],
            span,
        }
    }
    pub fn boolean(span: Span) -> Self {
        Self::Constant {
            name: TypeName::Bool,
            parameters: vec![],
            span,
        }
    }
    pub fn zone(span: Span) -> Self {
        Self::Constant {
            name: TypeName::Zone,
            parameters: vec![],
            span,
        }
    }
    pub fn card(span: Span) -> Self {
        Self::Constant {
            name: TypeName::Card,
            parameters: vec![],
            span,
        }
    }
    pub fn option(ty: Type, span: Span) -> Self {
        Self::Constant {
            name: TypeName::Option,
            parameters: vec![ty],
            span,
        }
    }
    pub fn array(ty: Type, span: Span) -> Self {
        Self::Constant {
            name: TypeName::Array,
            parameters: vec![ty],
            span,
        }
    }

    pub fn function(tys: Vec<Type>, span: Span) -> Self {
        Self::Constant {
            name: TypeName::Fn,
            parameters: tys,
            span,
        }
    }
}

impl<TV> GenericType<TV> {
    pub fn type_var(ty: TV, span: Span) -> Self {
        Self::Var(ty, span)
    }
}
