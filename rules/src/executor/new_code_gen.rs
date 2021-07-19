use std::{collections::HashMap, convert::TryInto, iter::once};

use crate::{
    executor::{
        bytecode::{ExecutableBytecode, LabeledBytecode},
        semantic_analysis::hm::TypedAst,
        value::Value,
    },
    parsing::FunctionDefinition,
};

use super::bytecode::InternalBytecode;

#[derive(Debug)]
struct SymbolTable {
    next_id: usize,
    next_anon_fn: usize,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            next_id: 0,
            next_anon_fn: 0,
        }
    }
}

impl SymbolTable {
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

    pub fn get_label_for(&self, name: &str) -> Option<&str> {
        todo!()
    }
}

fn generate_internal(ast: TypedAst<'_>, symbols: &mut SymbolTable) -> Vec<LabeledBytecode> {
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
                    let mut _callee_code = generate_internal(callee, symbols);
                    _callee_code.insert(0, LabeledBytecode::Label(label.clone()));

                    vec![LabeledBytecode::Call(label)]
                }
                // emits Call with the properlabel if available otherwise emits a std binding load + CallDynamic
                TypedAst::Binding { name, .. } => {
                    if let Some(label) = symbols.get_label_for(*name) {
                        vec![LabeledBytecode::Call(label.to_string())]
                    } else {
                        vec![
                            LabeledBytecode::LoadRef(name.to_string()),
                            LabeledBytecode::CallDynamic,
                        ]
                    }
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
            //
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

        TypedAst::Value { ty: _, value } => {
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
        TypedAst::Eval { children, span, ty } => todo!(),
        TypedAst::Let {
            bindings,
            expr,
            span,
            ty,
        } => todo!(),
        TypedAst::Fn {
            bindings,
            return_type,
            expr,
            span,
            ty,
        } => todo!(),
        // Sexpr::Eval {
        //     target,
        //     mut arguments,
        //     ..
        // } => match target {
        //     "print" => {
        //         let mut res = generate_internal(arguments.remove(0), symbols);
        //         res.push(LabeledBytecode::print());
        //         res
        //     }
        //     _ => {
        //         let label = symbols
        //             .get_binding(target)
        //             .map(|label| vec![LabeledBytecode::call(label.to_string())])
        //             .unwrap_or_else(|| vec![LabeledBytecode::call(target.to_string())]);

        //         arguments
        //             .into_iter()
        //             .rev()
        //             .map(|arg| generate_internal(arg, symbols))
        //             .flatten()
        //             .chain(label)
        //             .collect::<Vec<_>>()
        //     }
        // },
        // Sexpr::Symbol(binding, ..) => {
        //     vec![LabeledBytecode::load_ref(
        //         symbols.get_binding(binding).unwrap().to_string(),
        //     )]
        // }
        // Sexpr::Fn {
        //     eval, arguments, ..
        // } => {
        //     let anon_label = symbols.new_binding("#anon-fn#").to_string();

        //     let mut to_unload = vec![];
        //     let mut preamble = vec![LabeledBytecode::Label(anon_label.clone())];

        //     for (binding, _) in arguments {
        //         let binding = symbols.new_binding(binding).to_string();
        //         preamble.push(LabeledBytecode::store(binding.clone()));
        //         to_unload.push(LabeledBytecode::unload(binding));
        //     }

        //     preamble.extend(generate_internal(*eval, symbols));

        //     preamble.extend(to_unload);

        //     preamble.extend(vec![LabeledBytecode::ret()]);

        //     symbols.add_fn(preamble);

        //     vec![LabeledBytecode::LoadLabel(anon_label)]
        // }
        // Sexpr::Let { bindings, expr, .. } => {
        //     let mut result = vec![];
        //     let mut to_unload = vec![];
        //     symbols.new_scope();
        //     for (binding, value) in bindings {
        //         let data = generate_internal(value, symbols);
        //         if let Some(LabeledBytecode::LoadLabel(new_label)) = data.last() {
        //             // this means we just bound a function
        //             // let mut data = data;
        //             // data.insert(0, LabeledBytecode::label(binding.clone()));
        //             symbols.rebind(binding, new_label.clone());
        //         } else {
        //             let binding = symbols.new_binding(binding).to_string();
        //             result.extend(data);
        //             result.push(LabeledBytecode::store(binding.clone()));
        //             to_unload.push(LabeledBytecode::unload(binding));
        //         }
        //     }
        //     result.extend(generate_internal(*expr, symbols));
        //     result.extend(to_unload);
        //     symbols.end_scope();
        //     result
        // }
    }
}
pub fn generate(
    ast: TypedAst<'_>,
) -> (
    Vec<ExecutableBytecode>,
    Vec<LabeledBytecode>,
    HashMap<String, usize>,
) {
    // let mut symbols = SymbolTable::default();
    // let internal = generate_internal(ast, &mut symbols);
    // let internal = internal
    //     .into_iter()
    //     .chain(once(InternalBytecode::Return))
    //     .chain(
    //         function_defintions
    //             .into_iter()
    //             .map(|def| {
    //                 // when deffing a function, remove the anon function preamble
    //                 // also define own symbol table cause fuck you
    //                 let mut symbols = SymbolTable::default();
    //                 generate_internal(def.eval, &mut symbols);

    //                 symbols.fns.remove(0);

    //                 symbols
    //                     .fns
    //                     .insert(0, InternalBytecode::label(def.name.to_string()));

    //                 symbols.fns
    //             })
    //             .flatten(),
    //     )
    //     .collect::<Vec<_>>()
    //     .into_iter()
    //     .chain(symbols.fns)
    //     .collect::<Vec<_>>();

    // let label_values = internal
    //     .iter()
    //     .enumerate()
    //     .filter_map(|(idx, item)| {
    //         if let InternalBytecode::Label(value) = item {
    //             Some((value.to_string(), idx))
    //         } else {
    //             None
    //         }
    //     })
    //     .collect::<HashMap<_, _>>();

    // let mut next_binding = 0;
    // let mut bindings = HashMap::new();

    // (
    //     internal
    //         .iter()
    //         .map(|x| match x {
    //             InternalBytecode::Print => Bytecode::Print,
    //             InternalBytecode::Call(value) => Bytecode::Call(value.clone()),
    //             InternalBytecode::Load(value) => Bytecode::Load(value.clone()),
    //             InternalBytecode::LoadLabel(label) => {
    //                 Bytecode::LoadLabel(label_values[label.as_str()])
    //             }
    //             InternalBytecode::Label(value) => Bytecode::Label(value.clone()),
    //             InternalBytecode::Jump(label) => Bytecode::Jump(label_values[label.as_str()]),
    //             InternalBytecode::JumpIf(label) => Bytecode::JumpIf(label_values[label.as_str()]),
    //             InternalBytecode::Return => Bytecode::Return,
    //             InternalBytecode::Store(binding) => {
    //                 Bytecode::Store(*bindings.entry(binding).or_insert_with(|| {
    //                     let ret = next_binding;
    //                     next_binding += 1;
    //                     ret
    //                 }))
    //             }
    //             InternalBytecode::LoadRef(binding) => {
    //                 Bytecode::LoadRef(*bindings.entry(binding).or_insert_with(|| {
    //                     let ret = next_binding;
    //                     next_binding += 1;
    //                     ret
    //                 }))
    //             }
    //             InternalBytecode::Unload(binding) => {
    //                 Bytecode::Unload(*bindings.entry(binding).or_insert_with(|| {
    //                     let ret = next_binding;
    //                     next_binding += 1;
    //                     ret
    //                 }))
    //             }
    //             InternalBytecode::CallDynamic => Bytecode::CallDynamic,
    //         })
    //         .collect(),
    //     internal,
    //     label_values,
    // )
    todo!()
}
