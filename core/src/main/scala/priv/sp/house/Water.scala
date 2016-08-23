package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._

trait Water {
  import GameCardEffect._

  val Water = House("water", List(
    Spell("water.meditate", I18n("water.meditate.description"), effects = effects(Direct -> addMana(1, 0, 2, 3))),
    new Creature("water.sprite", Attack(5), 22, I18n("water.sprite.description"),
      effects = effects(OnTurn -> { env: Env ⇒
        env.focus()
        env.player.inflict(Damage(2, env, isAbility = true))
      })),
    new Creature("water.apostate", Attack(3), 10, I18n("water.apostate.description"), effects = effects(Direct -> focus(addMana(2, 0)))),
    new Creature("water.golem", Attack(4), 12, I18n("water.golem.description"), reaction = new GolemReaction),
    new Creature("water.elder", Attack(3), 16, I18n("water.elder.description"), effects = effects(OnTurn -> addMana(1, 2))), //, reaction = ManaGrowthReaction(1, 2)),
    new Creature("water.guard", Attack(3), 20, I18n("water.guard.description"), reaction = new IceguardReaction),
    new Creature("water.turtle", Attack(5), 16, I18n("water.turtle.description"), reaction = new TurtleReaction),
    Spell("water.shower", I18n("water.shower.description"), effects = effects(Direct -> massDamage(15, isSpell =true), Direct -> { env: Env ⇒
      env.otherPlayer.houses.incrMana(-1, 0, 1, 2, 3, 4)
    })),
    new Creature("water.lord", Attack(7), 34, I18n("water.lord.description"), reaction = new OverlordSlotReaction),
    new Creature("water.elemental", AttackSources().add(ManaAttack(1)), 38, I18n("water.elemental.description"), effects = effects(Direct -> focus(heal(10)), OnTurn -> addMana(1, 1))),
    new Creature("water.mind", Attack(6), 22, I18n("water.mind.description"), reaction = ManaGrowthReaction(1, 0, 1, 2, 3, 4)),
    new Creature("water.astral", Attack(1), 17, I18n("water.astral.description"), reaction = OppManaGrowthReaction(-1, 0, 1, 2, 3, 4))),
  houseIndex = 1)

  class GolemReaction extends Reaction {
    override def inflict(damage: Damage) {
      if (!damage.isEffect) { super.inflict(damage) }
    }
  }

  class TurtleReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      d.copy(amount = math.max(0, d.amount - 5))
    }
  }
}

private class OverlordSlotReaction extends Reaction {
  final override def onAdd(slot: SlotUpdate) = {
    if (math.abs(selected.num - slot.num) == 1) {
      slot toggle runFlag
    }
  }
}

private class IceguardReaction extends Reaction {
  override def onProtect(d: DamageEvent) = {
    if (d.target.isEmpty) {
      d.damage.copy(amount = math.ceil(d.damage.amount / 2.0).intValue)
    } else d.damage
  }
}
