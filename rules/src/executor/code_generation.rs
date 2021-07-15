use std::{collections::HashMap, convert::TryInto, iter::once};

use crate::{
    executor::{
        bytecode::{Bytecode, LabeledBytecode},
        value::Value,
    },
    parsing::SexprValue,
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

    pub fn add_fn<I: IntoIterator<Item = LabeledBytecode>>(&mut self, data: I) {
        self.fns.extend(data)
    }
}

fn generate_internal(ast: SexprValue<'_>, symbols: &mut SymbolTable) -> Vec<LabeledBytecode> {
    match ast {
        SexprValue::Sexpr {
            target,
            mut arguments,
            ..
        } => match target {
            "print" => {
                let mut res = generate_internal(arguments.remove(0), symbols);
                res.push(LabeledBytecode::print());
                res
            }
            _ => {
                let label = symbols
                    .get_binding(target)
                    .map(ToString::to_string)
                    .unwrap_or(target.to_string());

                arguments
                    .into_iter()
                    .rev()
                    .map(|arg| generate_internal(arg, symbols))
                    .flatten()
                    .chain(once(LabeledBytecode::call(label)))
                    .collect::<Vec<_>>()
            }
        },
        SexprValue::Symbol(binding, ..) => {
            vec![LabeledBytecode::load_ref(
                symbols.get_binding(binding).unwrap().to_string(),
            )]
        }
        SexprValue::Array { values, .. } => {
            let len = values.len();
            values
                .into_iter()
                .map(|inner| generate_internal(inner, symbols))
                .flatten()
                .chain(once(LabeledBytecode::load(Value::ArrayLength(len))))
                .collect::<Vec<_>>()
        }
        rest @ SexprValue::None(..)
        | rest @ SexprValue::Zone(..)
        | rest @ SexprValue::Bool(..)
        | rest @ SexprValue::Unit(..)
        | rest @ SexprValue::Integer(..) => vec![LabeledBytecode::load(rest.try_into().unwrap())],
        SexprValue::Fn {
            eval, arguments, ..
        } => {
            let mut to_unload = vec![];
            let mut preamble = vec![];

            for (binding, _) in arguments {
                let binding = symbols.new_binding(binding).to_string();
                preamble.push(LabeledBytecode::store(binding.clone()));
                to_unload.push(LabeledBytecode::unload(binding));
            }

            preamble.extend(generate_internal(*eval, symbols));

            preamble.extend(to_unload);

            preamble.extend(vec![LabeledBytecode::ret()]);

            preamble
        }
        SexprValue::Let { bindings, expr, .. } => {
            let mut result = vec![];
            let mut to_unload = vec![];
            symbols.new_scope();
            for (binding, value) in bindings {
                let binding = symbols.new_binding(binding).to_string();
                let data = generate_internal(value, symbols);
                if matches!(data.last(), Some(LabeledBytecode::Return)) {
                    // this means we just bound a function
                    let mut data = data;
                    data.insert(0, LabeledBytecode::label(binding.clone()));
                    symbols.add_fn(data);
                } else {
                    result.extend(data);
                    result.push(LabeledBytecode::store(binding.clone()));
                    to_unload.push(LabeledBytecode::unload(binding));
                }
            }
            result.extend(generate_internal(*expr, symbols));
            result.extend(to_unload);
            symbols.end_scope();
            result
        }
        SexprValue::If {
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
            if_false.push(InternalBytecode::label(true_label));
            let mut if_true = generate_internal(*if_true, symbols);
            // need to add "symbol table" etc so it can generate fresh labels
            if_true.push(InternalBytecode::label(end_label));
            vec![condition, if_false, if_true]
                .into_iter()
                .flatten()
                .collect()
        }
    }
}
pub fn generate(
    ast: SexprValue<'_>,
) -> (Vec<Bytecode>, Vec<LabeledBytecode>, HashMap<String, usize>) {
    let mut symbols = SymbolTable::default();
    let internal = generate_internal(ast, &mut symbols);
    let internal = internal
        .into_iter()
        .chain(once(InternalBytecode::Return))
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
                InternalBytecode::Print => Bytecode::Print,
                InternalBytecode::Call(value) => Bytecode::Call(value.clone()),
                InternalBytecode::Load(value) => Bytecode::Load(value.clone()),
                InternalBytecode::Label(value) => Bytecode::Label(value.clone()),
                InternalBytecode::Jump(label) => Bytecode::Jump(label_values[label.as_str()]),
                InternalBytecode::JumpIf(label) => Bytecode::JumpIf(label_values[label.as_str()]),
                InternalBytecode::Return => Bytecode::Return,
                InternalBytecode::Store(binding) => {
                    Bytecode::Store(*bindings.entry(binding).or_insert_with(|| {
                        let ret = next_binding;
                        next_binding += 1;
                        ret
                    }))
                }
                InternalBytecode::LoadRef(binding) => {
                    Bytecode::LoadRef(*bindings.entry(binding).or_insert_with(|| {
                        let ret = next_binding;
                        next_binding += 1;
                        ret
                    }))
                }
                InternalBytecode::Unload(binding) => {
                    Bytecode::Unload(*bindings.entry(binding).or_insert_with(|| {
                        let ret = next_binding;
                        next_binding += 1;
                        ret
                    }))
                }
            })
            .collect(),
        internal,
        label_values,
    )
}
