use crate::executor::semantic_analysis::hm::types::TypeVariable;

#[derive(Debug, Default, Clone)]
pub struct Fresh(usize);
impl Fresh {
    pub fn next_type_variable(&mut self) -> TypeVariable {
        let ret = self.0;
        self.0 += 1;
        TypeVariable::new(ret)
    }
}

impl Iterator for Fresh {
    type Item = TypeVariable;
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_type_variable())
    }
}
