pub mod model_ref;

use crate::model::model_ref::{CardRef, PlayerRef};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use strum::Display;
use strum::{EnumIter, EnumString};

pub type ZoneVec = Vec<CardId>;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Game {
    pub players: [Player; 2],
    pub turn: usize,
    pub active_player: usize,
}

impl Game {
    pub fn find_card(&self, card_id: CardId) -> Option<CardRef> {
        for (idx, player) in self.players.iter().enumerate() {
            let player_ref = PlayerRef::new(idx);
            for (zone, data) in player.zones.iter() {
                let zone_ref = player_ref.zone_ref(*zone);
                if let Some((index, _)) =
                    data.iter().enumerate().find(|(_, data)| **data == card_id)
                {
                    return Some(zone_ref.card_ref(card_id, index));
                }
            }
        }

        None
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Player {
    pub cards: HashMap<CardId, Card>,
    pub refresh_point: u32,
    pub zones: HashMap<ZoneId, ZoneVec>,
}

impl Player {}

#[derive(
    Debug, Serialize, Deserialize, Display, Clone, Copy, PartialEq, Eq, Hash, EnumIter, EnumString,
)]
#[strum(serialize_all = "snake_case")]
pub enum ZoneId {
    Hand,
    Deck,
    WaitingRoom,
    Stage,
    Memory,
    Stock,
    Clock,
    Level,
    Climax,
    Marker,
    Resolution,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Card {
    pub color: Color,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CardId(pub usize);

#[derive(Debug, Serialize, Deserialize, Display)]
pub enum Color {
    Yellow,
    Green,
    Red,
    Blue,
}

#[derive(Debug, Serialize, Deserialize, Display)]
pub enum CardType {
    Character,
    Event,
    Climax,
}

#[derive(Debug, Serialize, Deserialize, Display)]
pub enum CardState {
    Stand,
    Rest,
    Reverse,
}
