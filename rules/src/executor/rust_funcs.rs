use crate::{
    executor::{error::RuntimeError, value::Value, Executor},
    model::{model_ref::GameDeref, CardId, Game},
};

pub fn card(executor: &mut Executor, _: &mut Game) -> Result<(), RuntimeError> {
    let card: i32 = executor.stack.pop()?;
    executor.stack.push(Value::CardId(CardId(card as usize)));
    Ok(())
}

pub fn move_card(executor: &mut Executor, game: &mut Game) -> Result<(), RuntimeError> {
    let card = executor.stack.pop()?;
    let target_zone = executor.stack.pop()?;
    if let Some(card_ref) = game.find_card(card) {
        let zone = card_ref.zone().get_mut(game);
        zone.retain(|item| *item != card);
        executor.stack.push(card_ref.zone().id());

        let target_zone = card_ref.owner().zone_ref(target_zone).get_mut(game);
        target_zone.push(card);
    } else {
        panic!("shouldve found a card");
    }
    Ok(())
}

pub fn some(executor: &mut Executor, _: &mut Game) -> Result<(), RuntimeError> {
    let value = executor.stack.pop_any()?;
    executor.stack.push(Value::Some(Box::new(value)));
    Ok(())
}
