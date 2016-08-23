package priv.sp.house

import priv.sp._

trait Air {
  import CardSpec._
  import GameCardEffect._

  val Air = House("air", List(

    new Creature("air.fairy", Attack(4), 11, I18n("air.fairy.description"), mod = Some(new SpellMod(x ⇒ x + 1))),

    new Creature("air.griffin", Attack(3), 15, I18n("air.griffin.description"),
      effects = effects(Direct -> { env: Env ⇒
        if (env.getMana(2) > 4) env.otherPlayer inflict Damage(5, env)
      })),

    Spell("air.call", I18n("air.call.description"),
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> damageCreature(6, isSpell = true), Direct -> damage(6, isSpell = true))),

    new Creature("air.medic", Attack(4), 19, I18n("air.medic.description"),
      effects = effects(Direct -> { env: Env ⇒
        env.focus()
        env.player heal math.min(env.getMana(3), 10)
      })),

    new Creature("air.fence", Attack(0), 28, I18n("air.fence.description"), effects = effects(OnTurn -> focus(damage(4, isAbility = true)))),

    Spell("air.lightnin",
      (state : GameState, playerId : PlayerId) =>
        I18n.bundle.format("air.lightnin.description", (5+state.players(playerId).houses(2).mana).toString),
      effects = effects(Direct -> { env: Env ⇒ env.otherPlayer inflict Damage(5 + env.getMana(2), env, isSpell = true) })),

    new Creature("air.phoenix", Attack(7), 16, I18n("air.phoenix.description"), reaction = new PhoenixReaction),

    Spell("air.chain", I18n("air.chain.description"), effects = effects(Direct -> damageCreatures(9, isSpell = true), Direct -> damage(9, isSpell = true))),

    new Creature("air.cloud", Attack(4), 20, I18n("air.cloud.description"), runAttack = MultiTargetAttack),

    Spell("air.twister", I18n("air.twister.description"),
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> { env: Env ⇒ env.otherPlayer.slots(env.selected).overridableDestroy() })),

    new Creature("air.elemental", AttackSources().add(ManaAttack(2)), 44, I18n("air.elemental.description"), effects = effects(Direct -> focus(damage(8, isAbility = true)), OnTurn -> addMana(1, 2))),

    new Creature("air.titan", Attack(9), 40, I18n("air.titan.description"),
      effects = effects(Direct -> { env: Env ⇒
        env.focus()
        env.otherPlayer.slots(env.selected) inflict Damage(15, env, isAbility = true)
      }))), houseIndex = 2)

}

class PhoenixReaction extends Reaction {
  final override def onMyDeath(dead: Dead) {
    import dead._
    if (!dead.isDestroy && player.houses.value(0).mana > 9) {
      val slot = player.slots(num)
      slot add card
      if (dead.slot.has(CardSpec.runFlag)) {
        slot.toggle(CardSpec.runFlag)
      }
    }
  }
}
