import client/msg.{type Msg}
import lustre/effect.{type Effect}

pub fn none(model: model) {
  #(model, effect.none())
}

pub fn effect(model: model, effect: Effect(Msg)) {
  #(model, effect)
}

pub fn effects(model: model, effects: List(Effect(Msg))) {
  #(model, effect.batch(effects))
}

pub fn add_effect(tuple: #(model, Effect(Msg)), effect: Effect(Msg)) {
  let #(model, fst_effect) = tuple
  #(model, effect.batch([fst_effect, effect]))
}
