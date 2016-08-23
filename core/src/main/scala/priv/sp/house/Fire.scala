package priv.sp.house

import priv.sp._
import priv.sp.update._

trait Fire {
  import CardSpec._
  import GameCardEffect._

  val Fire = House("fire", List(
    new Creature("fire.goblin", Attack(4), 16, I18n("fire.goblin.description"),
      effects = effects(OnTurn -> goblinBerserker)),
    new Creature("fire.wall", Attack(0), 5, I18n("fire.wall.description"),
      effects = effects(Direct -> damageCreatures(5, isAbility = true))),
    new Creature("fire.monk", Attack(3), 13, I18n("fire.monk.description"), effects = effects(OnTurn -> addMana(1, 0))),// reaction = ManaGrowthReaction(1, 0)),
    new Creature("fire.drake", Attack(4), 18, I18n("fire.drake.description"),
      status = runFlag),
    new Creature("fire.orc", Attack(3), 16, I18n("fire.orc.description"),
      reaction = new OrcSlotReaction),
    Spell("fire.wave", I18n("fire.wave.description"),
      effects = effects(Direct -> damageCreatures(9, isSpell = true))),
    new Creature("fire.bull", Attack(6), 20, I18n("fire.bull.description"),
      reaction = new BullSlotReaction),
    new Creature("fire.blargl", Attack(8), 26, I18n("fire.blargl.description"),
      effects = effects(Direct -> massDamage(4, isAbility = true, immuneSelf = true))),
    Spell("fire.inferno", I18n("fire.inferno.description"),
      inputSpec = Some(SelectTargetCreature), effects = effects(Direct -> inferno)),
    new Creature("fire.elemental", AttackSources().add(ManaAttack(0)), 36,
      I18n("fire.elemental.description"),
      effects = effects(
        Direct -> damageCreatures(3, isAbility = true),
        Direct -> focus(damage(3, isAbility = true)),
        OnTurn -> addMana(1, 0))),
    Spell("fire.apocalypse",
      (state : GameState, playerId : PlayerId) =>
        I18n.bundle.format("fire.apocalypse.description", (8 + state.players(playerId).houses(0).mana).toString),
      effects = effects(Direct -> armageddon)),
    new Creature("fire.dragon", Attack(9), 40, I18n("fire.dragon.description"),
      mod = Some(new SpellMod(x ⇒ math.ceil(x * 1.5).intValue)))), houseIndex = 0)

  private def goblinBerserker = { env: Env ⇒
    val damage = Damage(2, env, isAbility = true)
    val targets = env.player.slots(env.selected).adjacentSlots filter (_.value.isDefined)
    if (targets.nonEmpty) {
      env.focus()
      targets foreach (_.inflict(damage))
    }
  }

  private def inferno = { env: Env ⇒
    import env._

    val damage = Damage(10, env, isSpell = true)
    otherPlayer.slots foreach { slot ⇒
      slot.inflict(
        if (slot.num == selected) Damage(18, env, isSpell = true) else damage)
    }
  }

  private def armageddon = { env: Env ⇒
    import env._

    val d = Damage(getMana(0) + 8, env, isSpell = true)
    env.otherPlayer inflict d
    massDamage(d.amount, isSpell = true)(env)
  }
}

case class OrcAttackBonus(orcPos: Int) extends AttackFunc with UniqueAttack { def apply(attack: Int) = attack + 2 }
case class BullAttackBonus(bullPos: Int) extends AttackFunc with UniqueAttack { def apply(attack: Int): Int = attack + 1 }

private class OrcSlotReaction extends AttackBonusReaction {
  final def cond(selected: Int, num: Int) = math.abs(selected - num) == 1
  def getBonus(selected: Int) = OrcAttackBonus(selected)
}

class BullSlotReaction extends AttackBonusReaction {
  final def cond(selected: Int, num: Int) = selected != num
  def getBonus(selected: Int) = BullAttackBonus(selected)
}

