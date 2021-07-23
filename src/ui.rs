use std::{
    fs::write,
    sync::mpsc,
    time::{Duration, Instant},
};

use egui::{
    menu::{bar, menu},
    CollapsingHeader, Color32, Label, ScrollArea, SelectableLabel, TextEdit, Ui,
};
use rules::{
    executor::{
        bytecode::LabeledBytecode,
        code_generation::{generate, Generated},
        error::{CompileError, RuntimeError},
        semantic_analysis::{self, hm::type_tree::TypedAst},
        Executor,
    },
    model::{Card, CardId, Game, ZoneId},
    parsing::{make_span, parse_included_file, parse_program, ParsedProgram, Sexpr, Span},
};
use semantic_analysis::semantic_analysis;
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

#[derive(Serialize, Deserialize)]
pub struct DebugUi {
    pub console_lines: Vec<String>,
    pub console_input: String,
    pub generated_code: String,
    pub game: Game,
    pub view: View,

    #[serde(skip)]
    pub compiled: Compilation,

    #[serde(skip, default = "std::sync::mpsc::channel")]
    pub chan: (mpsc::Sender<Compilation>, mpsc::Receiver<Compilation>),
}

impl Default for DebugUi {
    fn default() -> Self {
        Self {
            console_lines: Default::default(),
            console_input: Default::default(),
            generated_code: Default::default(),
            game: Default::default(),
            view: Default::default(),
            compiled: Default::default(),
            chan: mpsc::channel(),
        }
    }
}

pub struct Times {
    parsing: Duration,
    semantic: Duration,
    code_gen: Duration,
    total: Duration,
}

pub struct Compilation {
    taken: Times,
    originals: Vec<Span>,
    errors: Vec<CompileError>,

    parsed: Option<ParsedProgram>,
    typed: Option<TypedAst>,
    generated: Option<Generated>,
    executor: Executor,
}

impl Default for Compilation {
    fn default() -> Self {
        Self {
            taken: Times {
                parsing: Duration::from_secs(0),
                semantic: Duration::from_secs(0),
                code_gen: Duration::from_secs(0),
                total: Duration::from_secs(0),
            },
            originals: Default::default(),
            errors: Default::default(),
            parsed: Default::default(),
            typed: Default::default(),
            generated: Default::default(),
            executor: Default::default(),
        }
    }
}

fn try_compile(input: String) -> Compilation {
    let start = Instant::now();
    let mut errors: Vec<CompileError> = vec![];

    let input = make_span(input, "<editor>");

    let mut originals = vec![input.clone()];

    let parsing_start = Instant::now();
    let parsed = parse_program(input);
    let mut parsing = Instant::now() - parsing_start;

    let mut parsed = if let Err(error) = parsed {
        match error {
            rules::parsing::Err::Incomplete(_) => todo!(),
            rules::parsing::Err::Error(error) | rules::parsing::Err::Failure(error) => {
                errors.push(error.into())
            }
        }
        None
    } else {
        parsed.ok().map(|(_, program)| program)
    };

    let mut semantic = Duration::from_secs(0);

    let typed = parsed.as_mut().and_then(|parsed| {
        let (includes, include_errors) = parsed
            .includes
            .iter()
            .map(|include| {
                std::fs::read(&include.path.as_str())
                    .map_err(CompileError::from)
                    .and_then(|result| String::from_utf8(result).map_err(CompileError::from))
                    .map(|i| (include.path.clone(), i))
            })
            .partition::<Vec<_>, _>(Result::is_ok);
        errors.extend(include_errors.into_iter().map(|x| x.unwrap_err()));

        for (path, include) in includes.into_iter().map(Result::unwrap) {
            let include = make_span(include, path);
            originals.push(include.clone());

            let include_start = Instant::now();
            let parsed_include = parse_included_file(include);
            parsing += Instant::now() - include_start;

            let parsed_include = match parsed_include {
                Ok((_, data)) => data,
                Err(error) => {
                    match error {
                        rules::parsing::Err::Incomplete(_) => todo!(),
                        rules::parsing::Err::Error(error) | rules::parsing::Err::Failure(error) => {
                            errors.push(error.into())
                        }
                    };
                    continue;
                }
            };
            parsed.externs.extend(parsed_include.externs);
            parsed.defines.extend(parsed_include.defines);
        }

        if errors.is_empty() {
            let semantic_start = Instant::now();
            let ret = match semantic_analysis(&parsed) {
                Ok(ast) => Some(ast),
                Err(err) => {
                    errors.extend(err);
                    None
                }
            };

            semantic = Instant::now() - semantic_start;

            ret
        } else {
            None
        }
    });
    let mut code_gen = Duration::from_secs(0);
    let generated = typed.clone().map(|ast| {
        let generated_start = Instant::now();
        let ret = generate(ast, &parsed.as_ref().unwrap().externs);
        code_gen = Instant::now() - generated_start;

        ret
    });

    Compilation {
        originals,
        errors,
        parsed,
        typed,
        generated,
        executor: Executor::default(),
        taken: Times {
            parsing,
            semantic,
            code_gen,
            total: Instant::now() - start,
        },
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum View {
    GameState,
    Editor,
    Parsed,
    TypeChecked,
    Bytecode,
    // Executor,
}
impl Default for View {
    fn default() -> Self {
        Self::GameState
    }
}

impl DebugUi {
    pub fn compile(&mut self) {
        let send = self.chan.0.clone();
        let data = self.console_input.clone();
        std::thread::spawn(move || {
            send.send(try_compile(data)).unwrap();
        });
    }
    pub fn on_frame(&mut self) {
        if let Ok(compilation) = self.chan.1.try_recv() {
            self.compiled = compilation;
        }
    }
    pub fn update(&mut self, ctx: &egui::CtxRef, _frame: &mut epi::Frame<'_>) {
        egui::TopBottomPanel::top("menu bar").show(ctx, |ui| {
            bar(ui, |ui| {
                menu(ui, "View", |ui| {
                    ui.radio_value(&mut self.view, View::GameState, "Game State");
                    ui.radio_value(&mut self.view, View::Editor, "Editor");
                    ui.radio_value(&mut self.view, View::Parsed, "Parsed AST");
                    ui.radio_value(&mut self.view, View::TypeChecked, "Typed AST");
                    ui.radio_value(&mut self.view, View::Bytecode, "Bytecode");
                });
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| match self.view {
            View::GameState => self.game_state(ui),
            View::Editor => self.code_editor(ui),
            View::Parsed => self.parsed_viewer(ui),
            View::TypeChecked => self.typed_viewer(ui),
            View::Bytecode => self.bytecode_viewer(ui),
        });
    }

    fn bytecode_viewer(&mut self, ui: &mut Ui) {
        let game = &mut self.game;
        let executor = &mut self.compiled.executor;
        let output = &mut self.console_lines;
        if let Some(generated) = &self.compiled.generated {
            ui.columns(3, |ui| {
                ScrollArea::auto_sized().show(&mut ui[0], |ui| {
                    for (address, instruction) in generated.labeled.iter().enumerate() {
                        if address == executor.ip {
                            ui.separator();
                            ui.visuals_mut().override_text_color = Some(Color32::GREEN);
                        } else if executor.ip_stack.contains(&address) {
                            ui.visuals_mut().override_text_color = Some(Color32::RED);
                        } else {
                            ui.visuals_mut().override_text_color = None;
                        }

                        let widget = SelectableLabel::new(false, instruction.to_string())
                            .text_style(egui::TextStyle::Monospace);
                        if matches!(instruction, LabeledBytecode::Label(..)) {
                            ui.add(widget);
                        } else {
                            ui.indent("bytecode", |ui| ui.add(widget));
                        }
                    }
                });
                {
                    let ui = &mut ui[1];

                    let _ = ui.selectable_label(false, format!("ip = {}", executor.ip));
                    CollapsingHeader::new("IP Stack")
                        .default_open(true)
                        .show(ui, |ui| {
                            for ip in executor.ip_stack.iter().rev() {
                                let _ = ui.selectable_label(false, format!("{}", ip));
                            }
                        });
                    ui.separator();

                    CollapsingHeader::new("Heap")
                        .default_open(true)
                        .show(ui, |ui| {
                            for (name, values) in executor
                                .heap
                                .iter()
                                .filter(|(_, values)| !values.is_empty())
                            {
                                if values.len() > 1 {
                                    CollapsingHeader::new(format!(
                                        "`{}` = {}",
                                        name,
                                        values.last().unwrap()
                                    ))
                                    .id_source(ui.id().with(name))
                                    .show(ui, |ui| {
                                        for item in values.iter().take(values.len() - 1).rev() {
                                            let _ = ui.selectable_label(false, item.to_string());
                                        }
                                    });
                                } else {
                                    let _ = ui.selectable_label(
                                        false,
                                        format!("`{}` = {}", name, values.last().unwrap()),
                                    );
                                }
                            }
                        });
                    CollapsingHeader::new("Stack")
                        .default_open(true)
                        .show(ui, |ui| {
                            for item in executor.stack.iter() {
                                let _ = ui.selectable_label(false, item.to_string());
                            }
                        });
                }
                {
                    let ui = &mut ui[2];

                    ui.horizontal(|ui| {
                        if ui.button("Advance").clicked() {
                            match executor.advance(generated, game) {
                                Ok(res) => output.extend(res),
                                Err(err) => match err {
                                    RuntimeError::NoInstructionPointer => {
                                        executor.reset();
                                        output.push("execution ended".to_string())
                                    }
                                    rest => output.push(format!("{}", rest)),
                                },
                            }
                        }

                        if ui.button("Play").clicked() {
                            output.push("began playing".to_string());
                            loop {
                                match executor.advance(generated, game) {
                                    Ok(res) => output.extend(res),
                                    Err(err) => {
                                        match err {
                                            RuntimeError::NoInstructionPointer => {
                                                output.push("execution ended".to_string())
                                            }
                                            rest => output.push(format!("{}", rest)),
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                        if ui.button("Reset").clicked() {
                            executor.reset();
                            output.clear();
                        }
                    });
                    ui.separator();

                    for item in output.iter() {
                        ui.label(item);
                    }
                }
            });
        }
    }

    fn parsed_viewer(&mut self, ui: &mut Ui) {
        let parsed = if let Some(parsed) = &self.compiled.parsed {
            parsed
        } else {
            return;
        };

        ScrollArea::auto_sized().show(ui, |ui| {
            ui.collapsing("Sources", |ui| {
                for span in self.compiled.originals.iter() {
                    ui.collapsing(format!("{}", span.extra), |ui| {
                        ui.monospace(format!("{}", span.fragment()));
                    });

                    ui.separator();
                }
            });
            ui.collapsing("Externs", |ui| {
                for decl in parsed.externs.iter() {
                    ui.monospace(format!("{}: {}", decl.name, decl.type_scheme));
                }
            });
            ui.collapsing("Defines", |ui| {
                for (id, decl) in parsed.defines.iter().enumerate() {
                    CollapsingHeader::new(format!("{} - {}", decl.name, decl.span.extra))
                        .id_source(ui.id().with(id))
                        .show(ui, |ui| {
                            Self::view_parse(ui, &decl.eval, id);
                        });
                }
            });
            ui.separator();

            Self::view_parse(ui, &parsed.expr, 0);
        });
    }

    fn typed_viewer(&mut self, ui: &mut Ui) {
        let parsed = if let Some(parsed) = &self.compiled.typed {
            parsed
        } else {
            return;
        };

        ScrollArea::auto_sized().show(ui, |ui| {
            Self::view_typed(ui, &parsed, 0);
        });
    }

    fn view_typed(ui: &mut Ui, ast: &TypedAst, id: usize) {
        match ast {
            TypedAst::Eval { children, ty, .. } => {
                CollapsingHeader::new(format!("eval: {}", ty))
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        let mut target = true;
                        for (id, expr) in children.iter().enumerate() {
                            Self::view_typed(ui, expr, id);
                            if target {
                                ui.separator();
                                target = false;
                            }
                        }
                    });
            }
            TypedAst::Binding { name, ty, .. } => {
                ui.add(
                    SelectableLabel::new(false, format!("{}: {}", name, ty))
                        .text_style(egui::TextStyle::Monospace),
                );
            }
            TypedAst::Value { value, .. } => {
                ui.add(
                    SelectableLabel::new(false, format!("{}", value,))
                        .text_style(egui::TextStyle::Monospace),
                );
            }
            TypedAst::Array { values, ty, .. } => {
                CollapsingHeader::new(format!("array: {}", ty))
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        for (id, expr) in values.iter().enumerate() {
                            Self::view_typed(ui, expr, id);
                        }
                    });
            }
            TypedAst::Fn {
                bindings, expr, ty, ..
            } => {
                CollapsingHeader::new(format!("{}", ty))
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        for (binding, ty) in bindings.iter() {
                            ui.add(
                                SelectableLabel::new(false, format!("{}: {}", binding, ty))
                                    .text_style(egui::TextStyle::Monospace),
                            );
                        }

                        ui.separator();
                        Self::view_typed(ui, expr, id);
                    });
            }
            TypedAst::If {
                condition,
                if_true,
                if_false,
                ty,
                ..
            } => {
                CollapsingHeader::new(format!("if: {}", ty))
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        ui.collapsing(format!("condition: {}", condition.ty()), |ui| {
                            Self::view_typed(ui, &condition, id);
                        });
                        ui.collapsing(format!("then: {}", if_true.ty()), |ui| {
                            Self::view_typed(ui, &if_true, id);
                        });
                        ui.collapsing(format!("else: {}", if_false.ty()), |ui| {
                            Self::view_typed(ui, &if_false, id);
                        });
                    });
            }
            TypedAst::Let {
                bindings, expr, ty, ..
            } => {
                CollapsingHeader::new(format!("let: {}", ty))
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        for (binding, expr) in bindings.iter() {
                            CollapsingHeader::new(format!("{}: {}", binding, expr.ty())).show(
                                ui,
                                |ui| {
                                    Self::view_typed(ui, expr, id);
                                },
                            );
                        }
                        ui.separator();
                        Self::view_typed(ui, expr, id);
                    });
            }
            TypedAst::Seq {
                sub_expressions,
                ty,
                ..
            } => {
                CollapsingHeader::new(format!("seq: {}", ty))
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        for (id, expr) in sub_expressions.iter().enumerate() {
                            Self::view_typed(ui, expr, id);
                        }
                    });
            }
        }
    }

    fn view_parse(ui: &mut Ui, ast: &Sexpr, id: usize) {
        match ast {
            Sexpr::Eval { arguments, .. } => {
                CollapsingHeader::new("eval")
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        for (id, expr) in arguments.iter().enumerate() {
                            Self::view_parse(ui, expr, id);
                        }
                    });
            }
            Sexpr::Symbol(symbol, _) => {
                ui.add(
                    SelectableLabel::new(false, symbol.as_str())
                        .text_style(egui::TextStyle::Monospace),
                );
            }
            Sexpr::Integer(value, _) => {
                ui.add(
                    SelectableLabel::new(false, value.to_string())
                        .text_style(egui::TextStyle::Monospace),
                );
            }
            Sexpr::Bool(value, _) => {
                ui.add(
                    SelectableLabel::new(false, value.to_string())
                        .text_style(egui::TextStyle::Monospace),
                );
            }
            Sexpr::Zone(value, _) => {
                ui.add(
                    SelectableLabel::new(false, value.to_string())
                        .text_style(egui::TextStyle::Monospace),
                );
            }
            Sexpr::Unit(_) => {
                ui.add(SelectableLabel::new(false, "()").text_style(egui::TextStyle::Monospace));
            }
            Sexpr::None(_) => {
                ui.add(SelectableLabel::new(false, "none").text_style(egui::TextStyle::Monospace));
            }
            Sexpr::Array { values, .. } => {
                CollapsingHeader::new("array")
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        for (id, expr) in values.iter().enumerate() {
                            Self::view_parse(ui, expr, id);
                        }
                    });
            }
            Sexpr::Fn {
                arguments,
                return_type,
                eval,
                ..
            } => {
                CollapsingHeader::new("fn")
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        let return_type = return_type
                            .as_ref()
                            .map(ToString::to_string)
                            .unwrap_or_else(|| "()".to_string());

                        let arguments = arguments
                            .iter()
                            .map(|(binding, ty)| format!("{}: {}", binding, ty))
                            .intersperse(", ".to_string())
                            .collect::<String>();

                        ui.monospace(format!("declared: fn({}) -> {}", arguments, return_type));
                        Self::view_parse(ui, eval, id);
                    });
            }
            Sexpr::If {
                condition,
                if_true,
                if_false,
                ..
            } => {
                CollapsingHeader::new("if")
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        ui.collapsing("condition", |ui| {
                            Self::view_parse(ui, &condition, id);
                        });
                        ui.collapsing("then", |ui| {
                            Self::view_parse(ui, &if_true, id);
                        });
                        ui.collapsing("else", |ui| {
                            Self::view_parse(ui, &if_false, id);
                        });
                    });
            }
            Sexpr::Let { bindings, expr, .. } => {
                CollapsingHeader::new("let")
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        for (binding, expr) in bindings.iter() {
                            CollapsingHeader::new(binding.as_str()).show(ui, |ui| {
                                Self::view_parse(ui, expr, id);
                            });
                        }
                        Self::view_parse(ui, expr, id);
                    });
            }
            Sexpr::Seq {
                sub_expressions, ..
            } => {
                CollapsingHeader::new("seq")
                    .id_source(ui.id().with(id))
                    .show(ui, |ui| {
                        for (id, expr) in sub_expressions.iter().enumerate() {
                            Self::view_parse(ui, expr, id);
                        }
                    });
            }
        }
    }

    fn code_editor(&mut self, ui: &mut Ui) {
        ui.columns(2, |ui| {
            ui[0].vertical(|ui| {
                let size = ui.available_size();
                if ui
                    .add_sized(
                        size,
                        TextEdit::multiline(&mut self.console_input)
                            .hint_text("enter code here")
                            .code_editor(),
                    )
                    .changed()
                {
                    write("./saved.w", &bincode::serialize(self).unwrap()).unwrap();

                    self.compile();
                };
            });
            ui[1].vertical(|ui| {
                ui.label("Timing:");
                ui.indent("timing_indent", |ui| {
                    ui.label(format!("Parsing: {:?}", self.compiled.taken.parsing));
                    ui.label(format!(
                        "Semantic Analysis: {:?}",
                        self.compiled.taken.semantic
                    ));
                    ui.label(format!(
                        "Code Generation: {:?}",
                        self.compiled.taken.code_gen
                    ));
                    ui.label(format!(
                        "Miscellaneous: {:?}",
                        (self.compiled.taken.total
                            - self.compiled.taken.parsing
                            - self.compiled.taken.semantic
                            - self.compiled.taken.code_gen)
                    ));
                    ui.label(format!("Total: {:?}", self.compiled.taken.total));
                });
                ui.separator();
                for error in self.compiled.errors.iter().rev() {
                    ui.add(
                        Label::new(error.to_string())
                            .text_color(Color32::RED)
                            .monospace(),
                    );
                    ui.separator();
                }
            });
        });
    }

    fn game_state(&mut self, ui: &mut Ui) {
        ui.vertical_centered(|ui| {
            ui.heading("Game");
            ui.separator();
            ui.label(format!("turn_number: {}", self.game.turn));
            ui.label(format!("active_player: {}", self.game.active_player));
        });
        ui.columns(2, |columns| {
            for (idx, (ui, player)) in columns.iter_mut().zip(self.game.players.iter()).enumerate()
            {
                ui.heading(format!("Player {}", idx + 1));
                ui.separator();
                ui.indent(ui.id().with(idx), |ui| {
                    ui.label(format!("refresh_point: {}", player.refresh_point));
                    ui.separator();
                    for zone in ZoneId::iter() {
                        egui::CollapsingHeader::new(format!("{}", zone)).show(ui, |ui| {
                            for (id, card) in player.zones[&zone]
                                .iter()
                                .map(|item| (*item, &player.cards[item]))
                            {
                                card_display(ui, id, card);
                            }
                        });
                    }
                });
            }
        });
    }
}

fn card_display(ui: &mut Ui, id: CardId, card: &Card) {
    ui.collapsing(format!("Card {}", id.0), |ui| {
        ui.label(format!("Color: {}", card.color));
    });
    //
}
