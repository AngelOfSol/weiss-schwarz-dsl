use std::{collections::HashMap, fs::write};

use egui::{Color32, Layout, ScrollArea, TextEdit, Ui, Vec2};
use rules::{
    executor::{
        code_generation::generate, error::RuntimeError, semantic_analysis, Executor, ExecutorHeap,
        ExecutorStack,
    },
    model::{Card, CardId, Game, ZoneId},
    parsing::parse_sexpr_value,
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
}
impl DebugUi {
    pub fn update(&mut self, ctx: &egui::CtxRef, _frame: &mut epi::Frame<'_>) {
        egui::TopBottomPanel::bottom("console").show(ctx, |ui| {
            ui.columns(3, |columns| {
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
                        let result = parse_sexpr_value(&self.console_input.clone())
                            .map_err(|err| err.to_string())
                            .and_then(|(_, value)| {
                                let mut executor = Executor {
                                    stack: ExecutorStack::default(),
                                    heap: ExecutorHeap::default(),
                                    bytecode: vec![],
                                    ip: 0,
                                    ip_stack: vec![],
                                    labels: HashMap::new(),
                                };
                                semantic_analysis(&value, &executor)
                                    .map_err(|err| err.to_string())?;
                                let (for_exec, for_display, labels) = generate(value);
                                executor.labels = labels;
                                executor.bytecode = for_exec;

                                self.generated_code = for_display
                                    .iter()
                                    .map(|instruction| {
                                        //
                                        let ret = instruction.to_string();
                                        if !ret.starts_with('\'') {
                                            format!("\t{}", ret)
                                        } else {
                                            ret
                                        }
                                    })
                                    .collect::<Vec<_>>()
                                    .join("\n");

                                let mut values = Vec::new();
                                loop {
                                    match executor.advance(&mut self.game) {
                                        Err(RuntimeError::Unfinished(extra)) => {
                                            if extra.chars().any(|c| !c.is_whitespace()) {
                                                values.push(extra);
                                            }
                                        }
                                        Err(RuntimeError::NoInstructionPointer) => {
                                            break Ok(values.join("\n"))
                                        }
                                        rest => break rest,
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
                    let size = ui.available_size();
                    ScrollArea::from_max_height(200.0)
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
                                for right in self.console_lines.iter() {
                                    ui.separator();
                                    ui.with_layout(Layout::right_to_left(), |ui| {
                                        let value = right.as_ref().unwrap_or_else(|v| v);
                                        if matches!(right, Err(_)) {
                                            ui.colored_label(Color32::RED, value);
                                        } else {
                                            ui.label(value);
                                        }
                                    });
                                }
                                if scroll {
                                    ui.scroll_to_cursor(egui::Align::Min);
                                    ui.memory().id_data.insert(id, false);
                                }
                            });
                    });
                });
            });
        });

        egui::TopBottomPanel::top("game_state").show(ctx, |ui| {
            ui.vertical_centered(|ui| {
                ui.heading("Game");
                ui.separator();
                ui.label(format!("turn_number: {}", self.game.turn));
                ui.label(format!("active_player: {}", self.game.active_player));
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.columns(2, |columns| {
                for (idx, (ui, player)) in
                    columns.iter_mut().zip(self.game.players.iter()).enumerate()
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
        });
    }
}

fn card_display(ui: &mut Ui, id: CardId, card: &Card) {
    ui.collapsing(format!("Card {}", id.0), |ui| {
        ui.label(format!("Color: {}", card.color));
    });
    //
}
