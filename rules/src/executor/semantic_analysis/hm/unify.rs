use crate::executor::{
    error::TypeError,
    semantic_analysis::hm::{substitution::Substitution, types::Type},
};

pub fn unify<'a>(lhs: Type<'a>, rhs: Type<'a>) -> Result<Substitution<'a>, TypeError<'a>> {
    match (&lhs, &rhs) {
        (
            Type::Constant {
                name: lhs_name,
                parameters: lhs_params,
                ..
            },
            Type::Constant {
                name: rhs_name,
                parameters: rhs_params,
                span: rhs_span,
            },
        ) => {
            let mut err = None;

            if lhs_name != rhs_name || lhs_params.len() != rhs_params.len() {
                err = Some(TypeError::InvalidType {
                    expected: lhs.clone(),
                    found: rhs.clone(),
                    span: *rhs_span,
                });
            }
            let mut sub = Substitution::default();

            for (left, right) in lhs_params.iter().zip(rhs_params.iter()) {
                match unify(left.apply(&sub), right.apply(&sub)) {
                    Ok(applied) => sub = sub.union(applied),
                    Err(bubbled) => {
                        err = Some(bubbled);
                    }
                }
            }
            if let Some(err) = err {
                match err {
                    TypeError::InvalidType {
                        expected,
                        found,
                        span,
                    } => Err(TypeError::InvalidType {
                        expected: expected.apply(&sub),
                        found: found.apply(&sub),
                        span,
                    }),
                    err => Err(err),
                }
            } else {
                Ok(sub)
            }
        }
        (Type::Var(tvar, ..), rhs @ Type::Var(rhs_tvar, ..)) => {
            if tvar == rhs_tvar {
                Ok(Substitution::default())
            } else {
                Ok(Substitution {
                    map: vec![(*tvar, rhs.clone())],
                })
            }
        }
        (any, Type::Var(tvar, span)) | (Type::Var(tvar, span), any) => {
            if any.occurs(tvar) {
                Err(TypeError::InfiniteType {
                    left: Type::Var(*tvar, *span),
                    right: any.clone(),
                    span: *span,
                })
            } else {
                Ok(Substitution {
                    map: vec![(*tvar, any.clone())],
                })
            }
        }
    }
}
