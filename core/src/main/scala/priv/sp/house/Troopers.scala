package priv.sp.house

import priv.sp._

class Trooper {
  import CardSpec._
  import GameCardEffect._

  val Trooper = House("troopers", List(
    Spell("troopers.Conscription", I18n("troopers.Conscription.description"), effects = effects(Direct -> addMana(1, 4))),
    new Creature("troopers.Marine", Attack(4), 17, I18n("troopers.Marine.description"), reaction = new MarineReaction),
    new Creature("troopers.Barracks", Attack(2), 17, I18n("troopers.Barracks.description"), effects = effects(OnTurn -> addMana(1, 4))),
    new Creature("troopers.Wraith", Attack(4), 24, I18n("troopers.Wraith.description"), effects = effects(OnTurn -> focus(damageCreatures(2, isAbility = true)))),
    new Creature("troopers.Goliath", Attack(6), 20, I18n("troopers.Goliath.description"), reaction = new GoliathReaction),
    new Creature("troopers.SiegeTank", Attack(8), 29, I18n("troopers.SiegeTank.description"), effects = effects(OnTurn -> focus(siege))),
    Spell("troopers.NuclearMissile", I18n("troopers.NuclearMissile.description"), effects = effects(Direct -> damageCreatures(19, isSpell = true))),
    new Creature("troopers.ScienceVessel", Attack(6), 60, I18n("troopers.ScienceVessel.description"), effects = effects(Direct -> damageCreatures(12, isAbility = true)))))

  val marine = Trooper.cards(1)
  Trooper initCards { i: Int ⇒ if (i == 0) i else i + 1 }

  private def siege = { env: Env ⇒
    import env._

    otherPlayer.slots.filleds.sortBy(_.get.life).lastOption foreach { slot ⇒
      slot inflict Damage(8, env, isAbility = true)
    }
  }

  class GoliathReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      if (d.isEffect) d.copy(amount = 0) else d.copy(amount = math.max(0, d.amount - 1))
    }
  }

  class MarineReaction extends Reaction {

    final override def onSummon(summoned: SummonEvent) {
      import summoned._
      if (selected.playerId != player.id) {
        val damage = Damage(4, Context(selected.playerId, Some(marine), selected.num), isAbility = true)
        selected.focus()
        player.slots(num) inflict damage
      }
    }
  }
}
