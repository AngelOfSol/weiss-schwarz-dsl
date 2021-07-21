use std::{collections::HashMap, fs::write};

use egui::{
    menu::{bar, menu},
    Color32, Label, Response, ScrollArea, TextEdit, Ui, Vec2,
};
use rules::{
    executor::{
        bytecode::LabeledBytecode,
        code_generation::generate,
        error::{make_error_message, RuntimeError},
        semantic_analysis, Executor, Heap, Stack,
    },
    model::{Card, CardId, Game, ZoneId},
    parsing::{parse_included_file, parse_program, Span},
};
use semantic_analysis::semantic_analysis;
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

#[derive(Default, Serialize, Deserialize)]
pub struct DebugUi {
    pub console_lines: Vec<Result<String, String>>,
    pub console_input: String,
    pub generated_code: String,
    pub game: Game,
    pub view: View,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum View {
    Legacy,
    GameState,
    Editor,
}
impl Default for View {
    fn default() -> Self {
        Self::GameState
    }
}

impl DebugUi {
    pub fn update(&mut self, ctx: &egui::CtxRef, _frame: &mut epi::Frame<'_>) {
        egui::TopBottomPanel::top("menu bar").show(ctx, |ui| {
            bar(ui, |ui| {
                menu(ui, "View", |ui| {
                    ui.radio_value(&mut self.view, View::Legacy, "Legacy");
                    ui.radio_value(&mut self.view, View::GameState, "Game State");
                    ui.radio_value(&mut self.view, View::Editor, "Editor");
                });
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| match self.view {
            View::Legacy => self.legacy(ui),
            View::GameState => self.game_state(ui),
            View::Editor => self.code_editor(ui),
        });
    }
    fn code_editor(&mut self, ui: &mut Ui) {
        if ui
            .add(
                TextEdit::multiline(&mut self.console_input)
                    .hint_text("enter code here")
                    .code_editor(),
            )
            .changed()
        {
            self.console_input = self.console_input.replace("\t", "  ");
            write("./saved.w", &bincode::serialize(self).unwrap()).unwrap();
        };
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

    fn legacy(&mut self, ui: &mut Ui) {
        ui.vertical_centered(|ui| {
            ui.columns(3, |columns| {
                for c in columns.iter_mut() {
                    c.set_height(200.0);
                }
                columns[0].vertical(|ui| {
                    let id = ui.make_persistent_id("scroll");
                    ui.memory().id_data.get_or_insert_with(id, || true);

                    let size = ui.available_size() - Vec2::new(0.0, ui.spacing().interact_size.y);
                    ScrollArea::from_max_height(200.0 - ui.spacing().interact_size.y)
                        .id_source(ui.make_persistent_id("code editor"))
                        .show(ui, |ui| {
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
                            };
                        });

                    if ui.button("Run").clicked() && !self.console_input.is_empty() {
                        let temp = self.console_input.clone();
                        let ci = Span::new_extra(&temp, "<editor>");
                        let result = parse_program(ci)
                            .map_err(|err| match err {
                                rules::parsing::Err::Incomplete(_) => todo!(),
                                rules::parsing::Err::Failure(inner)
                                | rules::parsing::Err::Error(inner) => inner
                                    .errors
                                    .into_iter()
                                    .map(|(span, kind)| match kind {
                                        rules::parsing::VerboseErrorKind::Context(ctx) => {
                                            (span, ctx)
                                        }
                                        rules::parsing::VerboseErrorKind::Char(_)
                                        | rules::parsing::VerboseErrorKind::Nom(_) => {
                                            (span, "nom error")
                                        }
                                    })
                                    .map(|(span, kind)| {
                                        make_error_message(
                                            &format!("parsing error: {}", kind),
                                            kind,
                                            span,
                                            std::str::from_utf8(span.get_line_beginning()).unwrap(),
                                        )
                                    })
                                    .intersperse("\n".to_string())
                                    .collect::<String>(),
                            })
                            .and_then(|(_, (mut externs, mut defintions, includes, value))| {
                                let includes = includes
                                    .into_iter()
                                    .flat_map(|include| {
                                        std::fs::read(&include.path)
                                            .ok()
                                            .and_then(|result| String::from_utf8(result).ok())
                                            .map(|i| (include.path, i))
                                    })
                                    .collect::<Vec<_>>();

                                for (path, include) in includes.iter() {
                                    let (_, (included_externs, included_defintions)) =
                                        parse_included_file(Span::new_extra(&include, path))
                                            .map_err(|err| err.to_string())?;
                                    externs.extend(included_externs);
                                    defintions.extend(included_defintions);
                                }

                                let mut executor = Executor {
                                    stack: Stack::default(),
                                    heap: Heap::default(),
                                    ip: 0,
                                    ip_stack: vec![],
                                    labels: HashMap::new(),
                                };
                                let value = semantic_analysis(&value, &externs, &defintions)
                                    .map_err(|err| err.to_string())?;
                                let (for_exec, for_display, labels) = generate(value, &externs);
                                executor.labels = labels;

                                self.generated_code = for_display
                                    .iter()
                                    .map(|instruction| {
                                        if matches!(instruction, LabeledBytecode::Label(..)) {
                                            format!("\t{}", instruction)
                                        } else {
                                            instruction.to_string()
                                        }
                                    })
                                    .intersperse("\n".to_string())
                                    .collect();

                                let mut values = Vec::new();
                                loop {
                                    match executor.advance(&for_exec, &mut self.game) {
                                        Ok(Some(inner)) => {
                                            values.push(inner);
                                        }
                                        Ok(None) => (),
                                        Err(RuntimeError::NoInstructionPointer) => {
                                            break Ok(values.join("\n"))
                                        }
                                        Err(rest) => break Err(rest),
                                    }
                                }
                                .map_err(|err| err.to_string())
                            });

                        // scroll to bottom
                        ui.memory().id_data.insert(id, true);

                        self.console_lines.push(result);
                        if self.console_lines.len() > 20 {
                            let _ = self.console_lines.remove(0);
                        }
                        write("./saved.w", &bincode::serialize(self).unwrap()).unwrap();
                    };
                });
                columns[1].vertical(|ui| {
                    let size = ui.available_size_before_wrap_finite();
                    ScrollArea::auto_sized()
                        .id_source(ui.make_persistent_id("compilation output"))
                        .show(ui, |ui| {
                            ui.add_sized(
                                size,
                                TextEdit::multiline(&mut self.generated_code).enabled(false),
                            );
                        });
                });
                columns[2].vertical(|ui| {
                    let id = ui.make_persistent_id("scroll");
                    let scroll = *ui.memory().id_data.get_mut_or_insert_with(id, || true);
                    ui.allocate_ui(ui.available_size_before_wrap_finite(), |ui| {
                        ui.set_min_height(200.0);
                        ScrollArea::from_max_height(200.0)
                            .id_source(ui.make_persistent_id("runtime output"))
                            .show(ui, |ui| {
                                ui.indent("runtime output indent", |ui| {
                                    for right in self.console_lines.iter() {
                                        ui.separator();
                                        let value = right.as_ref().unwrap_or_else(|v| v);
                                        if matches!(right, Err(_)) {
                                            ui.add(
                                                Label::new(value)
                                                    .text_color(Color32::RED)
                                                    .monospace(),
                                            );
                                        } else {
                                            ui.label(value);
                                        }
                                    }
                                });
                                if scroll {
                                    ui.scroll_to_cursor(egui::Align::Min);
                                    ui.memory().id_data.insert(id, false);
                                }
                            });
                    });
                });
            });
        });
    }
}

fn card_display(ui: &mut Ui, id: CardId, card: &Card) {
    ui.collapsing(format!("Card {}", id.0), |ui| {
        ui.label(format!("Color: {}", card.color));
    });
    //
}
