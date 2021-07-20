use crate::executor::semantic_analysis::hm::types::TypeVariable;

#[derive(Debug, Default, Clone)]
pub struct Fresh(usize);
impl Fresh {
    pub fn next(&mut self) -> TypeVariable {
        let ret = self.0;
        self.0 += 1;
        TypeVariable::new(ret)
    }
}
