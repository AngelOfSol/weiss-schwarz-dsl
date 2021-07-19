use std::{collections::HashMap, iter::once};

use crate::{
    executor::{
        bytecode::{ExecutableBytecode, LabeledBytecode},
        semantic_analysis::hm::TypedAst,
        value::{Label, Value},
    },
    parsing::ExternDeclaration,
};

#[derive(Debug, Default)]
struct SymbolTable<'a> {
    next_id: usize,
    next_anon_fn: usize,
    next_fn_salt: usize,

    fn_labels: Vec<HashMap<&'a str, String>>,

    fn_extra_code: Vec<LabeledBytecode>,
}

impl<'a> SymbolTable<'a> {
    pub fn next_if_labels(&mut self) -> (String, String) {
        let idx = self.next_id;
        self.next_id += 1;

        (format!("if-true-#{}", idx), format!("end-if-#{}", idx))
    }
    pub fn next_anon_fn(&mut self) -> String {
        let idx = self.next_id;
        self.next_anon_fn += 1;

        format!("#anon-fn#-{}", idx)
    }

    pub fn get_label_for(&self, name: &str) -> Option<&String> {
        self.fn_labels.iter().rev().find_map(|map| map.get(name))
    }

    pub fn new_scope(&mut self, names: &[&'a str]) -> Vec<(&'a str, String)> {
        self.fn_labels.push(HashMap::new());

        names
            .iter()
            .map(|name| {
                let label = if self.get_label_for(*name).is_some() {
                    let salted = format!("{}#{}", name, self.next_fn_salt);
                    self.next_fn_salt += 1;
                    salted
                } else {
                    name.to_string()
                };
                let top = self.fn_labels.last_mut().unwrap();

                top.insert(name, label.clone());

                (*name, label)
            })
            .collect()
    }

    pub fn pop_scope(&mut self) {
        self.fn_labels.pop();
    }
}

fn generate_internal<'a>(ast: TypedAst<'a>, symbols: &mut SymbolTable<'a>) -> Vec<LabeledBytecode> {
    match ast {
        TypedAst::Eval { mut children, .. } => {
            let callee = children.remove(0);

            let mut arguments = children
                .into_iter()
                .rev()
                .map(|arg| generate_internal(arg, symbols))
                .flatten()
                .collect::<Vec<_>>();

            match &callee {
                TypedAst::Fn { .. } => {
                    let label = symbols.next_anon_fn();
                    let mut callee_code = generate_internal(callee, symbols);
                    callee_code.insert(0, LabeledBytecode::Label(label.clone()));

                    symbols.fn_extra_code.extend(callee_code);

                    arguments.push(LabeledBytecode::Call(label));

                    arguments
                }
                // emits Call with the properlabel if available otherwise emits a std binding load + CallDynamic
                TypedAst::Binding { name, .. } => {
                    arguments.extend(if let Some(label) = symbols.get_label_for(*name) {
                        vec![LabeledBytecode::Call(label.to_string())]
                    } else {
                        vec![
                            LabeledBytecode::LoadRef(name.to_string()),
                            LabeledBytecode::CallDynamic,
                        ]
                    });
                    arguments
                }
                TypedAst::Value { .. } => {
                    panic!("can't generate code with a value in the function position")
                }
                // this should emit call dynamic as the resulting code should evaluate to a Value::Label
                _ => {
                    arguments.extend(generate_internal(callee, symbols));
                    arguments.push(LabeledBytecode::CallDynamic);

                    arguments
                }
            }
        }
        TypedAst::Array { values, .. } => {
            let len = values.len();
            values
                .into_iter()
                .map(|inner| generate_internal(inner, symbols))
                .flatten()
                .chain(once(LabeledBytecode::Load(Value::ArrayLength(len))))
                .collect::<Vec<_>>()
        }

        TypedAst::Value { value, .. } => {
            vec![LabeledBytecode::Load(value)]
        }
        TypedAst::Binding { name, .. } => vec![LabeledBytecode::LoadRef(name.to_string())],
        TypedAst::Seq {
            sub_expressions, ..
        } => sub_expressions
            .into_iter()
            .flat_map(|expr| generate_internal(expr, symbols))
            .collect(),

        TypedAst::If {
            condition,
            if_true,
            if_false,
            ..
        } => {
            let (true_label, end_label) = symbols.next_if_labels();
            let mut condition = generate_internal(*condition, symbols);
            condition.push(LabeledBytecode::JumpIf(true_label.clone()));
            let mut if_false = generate_internal(*if_false, symbols);
            if_false.push(LabeledBytecode::Jump(end_label.clone()));
            if_false.push(LabeledBytecode::Label(true_label));
            let mut if_true = generate_internal(*if_true, symbols);
            // need to add "symbol table" etc so it can generate fresh labels
            if_true.push(LabeledBytecode::Label(end_label));
            vec![condition, if_false, if_true]
                .into_iter()
                .flatten()
                .collect()
        }
        TypedAst::Fn { bindings, expr, .. } => {
            let mut to_unload = vec![];
            let mut preamble = vec![];

            for (binding, _) in bindings {
                let binding = binding.to_string();
                preamble.push(LabeledBytecode::Store(binding.clone()));
                to_unload.push(LabeledBytecode::Unload(binding));
            }

            preamble.extend(generate_internal(*expr, symbols));

            preamble.extend(to_unload);

            preamble.extend(vec![LabeledBytecode::Return]);

            preamble
        }
        TypedAst::Let { bindings, expr, .. } => {
            //
            let new_scoped_labels = bindings
                .iter()
                .filter_map(|(name, ast)| {
                    if matches!(ast, TypedAst::Fn { .. }) {
                        Some(*name)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            let scoped = symbols.new_scope(&new_scoped_labels);

            let mut unload = vec![];

            let mut preamble = bindings
                .into_iter()
                .map(|(name, ast)| {
                    if matches!(ast, TypedAst::Fn { .. }) {
                        let (_, label) = scoped
                            .iter()
                            .find(|(original_binding, _)| original_binding == &name)
                            .unwrap();

                        let mut callee_code = generate_internal(ast, symbols);
                        callee_code.insert(0, LabeledBytecode::Label(label.clone()));

                        symbols.fn_extra_code.extend(callee_code);

                        // this load/store allows fns to be used first-class
                        let data = vec![
                            LabeledBytecode::LoadLabel(label.clone()),
                            LabeledBytecode::Store(name.to_string()),
                        ];

                        unload.push(LabeledBytecode::Unload(name.to_string()));

                        data
                    } else {
                        let mut data = generate_internal(ast, symbols);
                        data.push(LabeledBytecode::Store(name.to_string()));

                        unload.push(LabeledBytecode::Unload(name.to_string()));

                        data
                    }
                })
                .flatten()
                .collect::<Vec<_>>();

            preamble.extend(generate_internal(*expr, symbols));
            preamble.extend(unload);

            symbols.pop_scope();

            preamble
        }
    }
}
pub fn generate(
    ast: TypedAst<'_>,
    externs: &[ExternDeclaration],
) -> (
    Vec<ExecutableBytecode>,
    Vec<LabeledBytecode>,
    HashMap<String, usize>,
) {
    let mut symbols = SymbolTable::default();

    symbols.new_scope(&externs.iter().map(|decl| decl.name).collect::<Vec<_>>());

    let internal = generate_internal(ast, &mut symbols);

    let internal = internal
        .into_iter()
        .chain(once(LabeledBytecode::Return))
        .collect::<Vec<_>>()
        .into_iter()
        .chain(symbols.fn_extra_code)
        .collect::<Vec<_>>();

    let label_values = internal
        .iter()
        .enumerate()
        .filter_map(|(idx, item)| {
            if let LabeledBytecode::Label(value) = item {
                Some((value.to_string(), idx))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();

    (
        internal
            .iter()
            .map(|x| match x {
                LabeledBytecode::Print => ExecutableBytecode::Print,
                LabeledBytecode::Call(value) if value == "print" => ExecutableBytecode::Print,
                LabeledBytecode::Call(value) => ExecutableBytecode::Call(value.clone()),
                LabeledBytecode::Load(value) => ExecutableBytecode::Load(value.clone()),
                LabeledBytecode::LoadLabel(label) => {
                    ExecutableBytecode::Load(Value::Label(Label {
                        name: label.clone(),
                        ip: label_values[label.as_str()],
                    }))
                }
                LabeledBytecode::Label(value) => ExecutableBytecode::Label(value.clone()),
                LabeledBytecode::Jump(label) => {
                    ExecutableBytecode::Jump(label_values[label.as_str()])
                }
                LabeledBytecode::JumpIf(label) => {
                    ExecutableBytecode::JumpIf(label_values[label.as_str()])
                }
                LabeledBytecode::Return => ExecutableBytecode::Return,
                LabeledBytecode::Store(binding) => ExecutableBytecode::Store(binding.clone()),
                LabeledBytecode::LoadRef(binding) => ExecutableBytecode::LoadRef(binding.clone()),
                LabeledBytecode::Unload(binding) => ExecutableBytecode::Unload(binding.clone()),
                LabeledBytecode::CallDynamic => ExecutableBytecode::CallDynamic,
            })
            .collect(),
        internal,
        label_values,
    )
}
