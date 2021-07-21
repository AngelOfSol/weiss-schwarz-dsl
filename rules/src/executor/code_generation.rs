use std::{
    collections::{HashMap, HashSet},
    iter::once,
};

use arcstr::Substr;

use crate::{
    executor::{
        bytecode::{ExecutableBytecode, LabeledBytecode},
        semantic_analysis::hm::TypedAst,
        value::{Label, Value},
        RUST_FN,
    },
    parsing::ExternDeclaration,
};

#[derive(Debug, Default)]
struct SymbolTable {
    next_id: usize,
    next_anon_fn: usize,
    next_fn_salt: usize,

    fn_labels: Vec<HashMap<Substr, Substr>>,

    fn_used_labels: HashSet<Substr>,
    fn_extra_code: Vec<LabeledBytecode>,
}

impl SymbolTable {
    pub fn next_if_labels(&mut self) -> (Substr, Substr) {
        let idx = self.next_id;
        self.next_id += 1;

        (
            format!("if-true-#{}", idx).into(),
            format!("end-if-#{}", idx).into(),
        )
    }
    pub fn next_anon_fn(&mut self) -> Substr {
        let idx = self.next_id;
        self.next_anon_fn += 1;

        format!("#anon-fn#-{}", idx).into()
    }

    pub fn get_label_for(&self, name: &str) -> Option<&Substr> {
        self.fn_labels.iter().rev().find_map(|map| map.get(name))
    }

    pub fn new_scope(&mut self, names: &[Substr]) -> Vec<(Substr, Substr)> {
        self.fn_labels.push(HashMap::new());

        names
            .iter()
            .map(|name| {
                let label = if self.fn_used_labels.contains(name) {
                    let salted = format!("{}#{}", name, self.next_fn_salt);
                    // we wanna explicitly panic if we cap out on salts, even though
                    // its unlikely we ever will
                    self.next_fn_salt = self.next_fn_salt.checked_add(1).unwrap();
                    salted
                } else {
                    self.fn_used_labels.insert(name.clone());
                    name.to_string()
                };
                let label: Substr = label.into();
                let top = self.fn_labels.last_mut().unwrap();

                top.insert(name.clone(), label.clone());

                (name.clone(), label)
            })
            .collect()
    }

    pub fn pop_scope(&mut self) {
        self.fn_labels.pop();
    }
}

fn generate_internal(ast: TypedAst, symbols: &mut SymbolTable) -> Vec<LabeledBytecode> {
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
                // this means we're calling a fn directly, so we generate the code and assign it to an anonymous function
                TypedAst::Fn { .. } => {
                    let label = symbols.next_anon_fn();
                    let mut callee_code = generate_internal(callee, symbols);
                    callee_code.insert(0, LabeledBytecode::Label(label.clone()));

                    symbols.fn_extra_code.extend(callee_code);

                    arguments.push(LabeledBytecode::Call(label));

                    arguments
                }
                // if we have a label for the given binding in our scope, then we call that label directly
                // otherwise we have to load the binding manually, and then call-dynamic with the label on the stack
                TypedAst::Binding { name, .. } => {
                    arguments.extend(if let Some(label) = symbols.get_label_for(name) {
                        vec![LabeledBytecode::Call(label.clone())]
                    } else {
                        vec![
                            LabeledBytecode::LoadRef(name.clone()),
                            LabeledBytecode::CallDynamic,
                        ]
                    });
                    arguments
                }
                // we should never have a value in this position, as it's impossible to label things directly
                TypedAst::Value { .. } => {
                    panic!("can't generate code with a value in the function position")
                }
                // the resulting code should evaluate to a Value::Label(..) so we just run that code
                // and then emit a call-dynamic
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
        TypedAst::Binding { name, .. } => vec![LabeledBytecode::LoadRef(name)],
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
        // for fns we just write the pre/postamble and emit the code directly
        // the parent node will take care of properly labeling
        TypedAst::Fn { bindings, expr, .. } => {
            let mut to_unload = vec![];
            let mut preamble = vec![];

            for (binding, _) in bindings {
                preamble.push(LabeledBytecode::Store(binding.clone()));
                to_unload.push(LabeledBytecode::Unload(binding));
            }

            preamble.extend(generate_internal(*expr, symbols));

            preamble.extend(to_unload);

            preamble.extend(vec![LabeledBytecode::Return]);

            preamble
        }
        TypedAst::Let { bindings, expr, .. } => {
            // if we're binding to a fn, we create labels based on the binding
            // for the most part label == binding_name
            // in the case that the it already exists, we'll generate a new label for it
            let new_scoped_labels = bindings
                .iter()
                .filter_map(|(name, ast)| {
                    if matches!(ast, TypedAst::Fn { .. }) {
                        Some(name.clone())
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
                        // for FNs we have to generate the code, and then apply the label we generated
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
                            LabeledBytecode::Store(name.clone()),
                        ];

                        unload.push(LabeledBytecode::Unload(name));

                        data
                    } else {
                        let mut data = generate_internal(ast, symbols);
                        data.push(LabeledBytecode::Store(name.clone()));

                        unload.push(LabeledBytecode::Unload(name));

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
    ast: TypedAst,
    externs: &[ExternDeclaration],
) -> (
    Vec<ExecutableBytecode>,
    Vec<LabeledBytecode>,
    HashMap<String, usize>,
) {
    let mut symbols = SymbolTable::default();

    symbols.new_scope(
        &externs
            .iter()
            .map(|decl| decl.name.clone())
            .collect::<Vec<_>>(),
    );

    let externs = externs
        .into_iter()
        .filter(|decl| decl.name != "print")
        .map(|decl| {
            vec![
                LabeledBytecode::Load(Value::RustFn(
                    RUST_FN.get_key_value(decl.name.as_str()).unwrap().0,
                )),
                LabeledBytecode::Store(decl.name.clone()),
            ]
        })
        .flatten();

    let internal = generate_internal(ast, &mut symbols);

    let internal = externs
        .chain(internal)
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
                LabeledBytecode::Call(value) => ExecutableBytecode::Call(value.to_string()),
                LabeledBytecode::Load(value) => ExecutableBytecode::Load(value.clone()),
                LabeledBytecode::LoadLabel(label) => {
                    ExecutableBytecode::Load(Value::Label(Label {
                        name: label.to_string(),
                        ip: label_values[label.as_str()],
                    }))
                }
                LabeledBytecode::Label(value) => ExecutableBytecode::Label(value.to_string()),
                LabeledBytecode::Jump(label) => {
                    ExecutableBytecode::Jump(label_values[label.as_str()])
                }
                LabeledBytecode::JumpIf(label) => {
                    ExecutableBytecode::JumpIf(label_values[label.as_str()])
                }
                LabeledBytecode::Return => ExecutableBytecode::Return,
                LabeledBytecode::Store(binding) => ExecutableBytecode::Store(binding.to_string()),
                LabeledBytecode::LoadRef(binding) => {
                    ExecutableBytecode::LoadRef(binding.to_string())
                }
                LabeledBytecode::Unload(binding) => ExecutableBytecode::Unload(binding.to_string()),
                LabeledBytecode::CallDynamic => ExecutableBytecode::CallDynamic,
            })
            .collect(),
        internal,
        label_values,
    )
}
