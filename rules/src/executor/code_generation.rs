use std::{collections::HashMap, convert::TryInto, iter::once};

use crate::{
    executor::{
        bytecode::{ExecutableBytecode, LabeledBytecode},
        value::Value,
    },
    parsing::{FunctionDefinition, Sexpr},
};

use super::bytecode::InternalBytecode;

#[derive(Debug)]
struct SymbolTable {
    next_label: usize,
    next_binding: usize,
    variable_binding: Vec<HashMap<String, String>>,
    fns: Vec<LabeledBytecode>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            next_label: 0,
            next_binding: 0,
            variable_binding: vec![HashMap::new()],
            fns: vec![],
        }
    }
}

impl SymbolTable {
    pub fn next_if_label(&mut self) -> (String, String) {
        let idx = self.next_label;
        self.next_label += 1;

        (format!("if-true-{}", idx), format!("end-if-{}", idx))
    }

    pub fn new_scope(&mut self) {
        self.variable_binding
            .push(self.variable_binding.last().unwrap().clone());
    }
    pub fn end_scope(&mut self) {
        self.variable_binding.pop().unwrap();
    }
    pub fn get_binding(&mut self, binding: &str) -> Option<&str> {
        self.variable_binding
            .last()
            .and_then(|inner| inner.get(binding))
            .map(|value| value.as_str())
    }
    pub fn new_binding(&mut self, binding: &str) -> &str {
        let idx = self.next_binding;
        self.next_binding += 1;
        self.variable_binding
            .last_mut()
            .unwrap()
            .insert(binding.to_string(), format!("{}-{}", binding, idx));
        self.variable_binding.last().unwrap().get(binding).unwrap()
    }
    pub fn rebind(&mut self, old: &str, new: String) {
        self.variable_binding
            .last_mut()
            .unwrap()
            .insert(old.to_string(), new);
    }

    pub fn add_fn<I: IntoIterator<Item = LabeledBytecode>>(&mut self, data: I) {
        self.fns.extend(data)
    }
}

fn generate_internal(ast: Sexpr<'_>, symbols: &mut SymbolTable) -> Vec<LabeledBytecode> {
    match ast {
        Sexpr::Eval {
            target,
            mut arguments,
            ..
        } => match target {
            "print" => {
                let mut res = generate_internal(arguments.remove(0), symbols);
                res.push(LabeledBytecode::Print);
                res
            }
            _ => {
                let label = symbols
                    .get_binding(target)
                    .map(|label| vec![LabeledBytecode::Call(label.to_string())])
                    .unwrap_or_else(|| vec![LabeledBytecode::Call(target.to_string())]);

                arguments
                    .into_iter()
                    .rev()
                    .map(|arg| generate_internal(arg, symbols))
                    .flatten()
                    .chain(label)
                    .collect::<Vec<_>>()
            }
        },
        Sexpr::Symbol(binding, ..) => {
            vec![LabeledBytecode::LoadRef(
                symbols.get_binding(binding).unwrap().to_string(),
            )]
        }
        Sexpr::Array { values, .. } => {
            let len = values.len();
            values
                .into_iter()
                .map(|inner| generate_internal(inner, symbols))
                .flatten()
                .chain(once(LabeledBytecode::Load(Value::ArrayLength(len))))
                .collect::<Vec<_>>()
        }
        rest @ Sexpr::None(..)
        | rest @ Sexpr::Zone(..)
        | rest @ Sexpr::Bool(..)
        | rest @ Sexpr::Unit(..)
        | rest @ Sexpr::Integer(..) => vec![LabeledBytecode::Load(rest.try_into().unwrap())],
        Sexpr::Fn {
            eval, arguments, ..
        } => {
            let anon_label = symbols.new_binding("#anon-fn#").to_string();

            let mut to_unload = vec![];
            let mut preamble = vec![LabeledBytecode::Label(anon_label.clone())];

            for (binding, _) in arguments {
                let binding = symbols.new_binding(binding).to_string();
                preamble.push(LabeledBytecode::Store(binding.clone()));
                to_unload.push(LabeledBytecode::Unload(binding));
            }

            preamble.extend(generate_internal(*eval, symbols));

            preamble.extend(to_unload);

            preamble.extend(vec![LabeledBytecode::Return]);

            symbols.add_fn(preamble);

            vec![LabeledBytecode::LoadLabel(anon_label)]
        }
        Sexpr::Let { bindings, expr, .. } => {
            let mut result = vec![];
            let mut to_unload = vec![];
            symbols.new_scope();
            for (binding, value) in bindings {
                let data = generate_internal(value, symbols);
                if let Some(LabeledBytecode::LoadLabel(new_label)) = data.last() {
                    // this means we just bound a function
                    // let mut data = data;
                    // data.insert(0, LabeledBytecode::label(binding.clone()));
                    symbols.rebind(binding, new_label.clone());
                } else {
                    let binding = symbols.new_binding(binding).to_string();
                    result.extend(data);
                    result.push(LabeledBytecode::Store(binding.clone()));
                    to_unload.push(LabeledBytecode::Unload(binding));
                }
            }
            result.extend(generate_internal(*expr, symbols));
            result.extend(to_unload);
            symbols.end_scope();
            result
        }
        Sexpr::If {
            condition,
            if_true,
            if_false,
            ..
        } => {
            let (true_label, end_label) = symbols.next_if_label();
            let mut condition = generate_internal(*condition, symbols);
            condition.push(InternalBytecode::JumpIf(true_label.clone()));
            let mut if_false = generate_internal(*if_false, symbols);
            if_false.push(InternalBytecode::Jump(end_label.clone()));
            if_false.push(InternalBytecode::Label(true_label));
            let mut if_true = generate_internal(*if_true, symbols);
            // need to add "symbol table" etc so it can generate fresh labels
            if_true.push(InternalBytecode::Label(end_label));
            vec![condition, if_false, if_true]
                .into_iter()
                .flatten()
                .collect()
        }
        Sexpr::Seq {
            sub_expressions, ..
        } => sub_expressions
            .into_iter()
            .flat_map(|expr| generate_internal(expr, symbols))
            .collect(),
    }
}
pub fn generate(
    ast: Sexpr<'_>,
    function_defintions: Vec<FunctionDefinition<'_>>,
) -> (
    Vec<ExecutableBytecode>,
    Vec<LabeledBytecode>,
    HashMap<String, usize>,
) {
    let mut symbols = SymbolTable::default();
    let internal = generate_internal(ast, &mut symbols);
    let internal = internal
        .into_iter()
        .chain(once(InternalBytecode::Return))
        .chain(
            function_defintions
                .into_iter()
                .map(|def| {
                    // when deffing a function, remove the anon function preamble
                    // also define own symbol table cause fuck you
                    let mut symbols = SymbolTable::default();
                    generate_internal(def.eval, &mut symbols);

                    symbols.fns.remove(0);

                    symbols
                        .fns
                        .insert(0, InternalBytecode::Label(def.name.to_string()));

                    symbols.fns
                })
                .flatten(),
        )
        .collect::<Vec<_>>()
        .into_iter()
        .chain(symbols.fns)
        .collect::<Vec<_>>();

    let label_values = internal
        .iter()
        .enumerate()
        .filter_map(|(idx, item)| {
            if let InternalBytecode::Label(value) = item {
                Some((value.to_string(), idx))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();

    let mut next_binding = 0;
    let mut bindings = HashMap::new();

    (
        internal
            .iter()
            .map(|x| match x {
                InternalBytecode::Print => ExecutableBytecode::Print,
                InternalBytecode::Call(value) => ExecutableBytecode::Call(value.clone()),
                InternalBytecode::Load(value) => ExecutableBytecode::Load(value.clone()),
                InternalBytecode::LoadLabel(label) => {
                    ExecutableBytecode::LoadLabel(label_values[label.as_str()])
                }
                InternalBytecode::Label(value) => ExecutableBytecode::Label(value.clone()),
                InternalBytecode::Jump(label) => {
                    ExecutableBytecode::Jump(label_values[label.as_str()])
                }
                InternalBytecode::JumpIf(label) => {
                    ExecutableBytecode::JumpIf(label_values[label.as_str()])
                }
                InternalBytecode::Return => ExecutableBytecode::Return,
                InternalBytecode::Store(binding) => {
                    ExecutableBytecode::Store(*bindings.entry(binding).or_insert_with(|| {
                        let ret = next_binding;
                        next_binding += 1;
                        ret
                    }))
                }
                InternalBytecode::LoadRef(binding) => {
                    ExecutableBytecode::LoadRef(*bindings.entry(binding).or_insert_with(|| {
                        let ret = next_binding;
                        next_binding += 1;
                        ret
                    }))
                }
                InternalBytecode::Unload(binding) => {
                    ExecutableBytecode::Unload(*bindings.entry(binding).or_insert_with(|| {
                        let ret = next_binding;
                        next_binding += 1;
                        ret
                    }))
                }
                InternalBytecode::CallDynamic => ExecutableBytecode::CallDynamic,
            })
            .collect(),
        internal,
        label_values,
    )
}
