use crate::model::{Card, CardId, Game, Player, ZoneId, ZoneVec};

pub trait GameDeref {
    type Target;
    fn get<'game>(&self, game: &'game Game) -> &'game Self::Target;
    fn get_mut<'game>(&self, game: &'game mut Game) -> &'game mut Self::Target;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PlayerRef {
    player: usize,
}
impl PlayerRef {
    pub fn new(player: usize) -> Self {
        Self { player }
    }

    pub fn zone_ref(&self, zone: ZoneId) -> ZoneRef {
        ZoneRef {
            player: *self,
            zone,
        }
    }
}

impl GameDeref for PlayerRef {
    type Target = Player;

    fn get<'game>(&self, game: &'game Game) -> &'game Self::Target {
        &game.players[self.player]
    }

    fn get_mut<'game>(&self, game: &'game mut Game) -> &'game mut Self::Target {
        &mut game.players[self.player]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ZoneRef {
    player: PlayerRef,
    zone: ZoneId,
}

impl ZoneRef {
    pub fn id(&self) -> ZoneId {
        self.zone
    }
    pub fn owner(&self) -> PlayerRef {
        self.player
    }

    pub fn card_ref(&self, card_id: CardId, index: usize) -> CardRef {
        CardRef {
            zone: *self,
            card_id,
            index,
        }
    }
}

impl GameDeref for ZoneRef {
    type Target = ZoneVec;

    fn get<'game>(&self, game: &'game Game) -> &'game Self::Target {
        let player = self.player.get(game);
        &player.zones[&self.zone]
    }

    fn get_mut<'game>(&self, game: &'game mut Game) -> &'game mut Self::Target {
        let player = self.player.get_mut(game);
        player.zones.get_mut(&self.zone).unwrap()
    }
}

pub struct CardRef {
    zone: ZoneRef,
    card_id: CardId,
    index: usize,
}

impl CardRef {
    pub fn owner(&self) -> PlayerRef {
        self.zone.owner()
    }
    pub fn zone(&self) -> ZoneRef {
        self.zone
    }

    pub fn id(&self) -> CardId {
        self.card_id
    }
}

impl GameDeref for CardRef {
    type Target = Card;

    fn get<'game>(&self, game: &'game Game) -> &'game Self::Target {
        &self.zone.owner().get(game).cards[&self.zone.get(game)[self.index]]
    }

    fn get_mut<'game>(&self, game: &'game mut Game) -> &'game mut Self::Target {
        let key = self.zone.get(game)[self.index];
        self.zone.owner().get_mut(game).cards.get_mut(&key).unwrap()
    }
}
