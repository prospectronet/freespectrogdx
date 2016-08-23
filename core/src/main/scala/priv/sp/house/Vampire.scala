package priv.sp.house

import priv.sp._
import priv.sp.update._
import priv.util.FuncDecorators

/**
 * Introduced bullshit:
 * acolyte -> listen to enemy's damage
 */
class Vampire {
  import CardSpec._
  import GameCardEffect._


  val acolyte = new Creature("vampires.acolyte", Attack(3), 21,
    I18n("vampires.acolyte.description"),
    reaction = new AcolyteReaction)

  val Vampire: House = House("vampires", List(
    Spell("vampires.flock", I18n("vampires.flock.description"),
      inputSpec = Some(SelectOwnerSlot),
      effects = effects(Direct -> darkFlock)),

    new Creature("vampires.noctule", Attack(5), 17, I18n("vampires.noctule.description"), runAttack = new NoctuleAttack),

    new Creature("vampires.ghoul", Attack(4), 24, I18n("vampires.ghoul.description"), runAttack = new GhoulAttack),

    acolyte,

    Spell("vampires.ties", I18n("vampires.ties.description"),
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> bloodTies)),

    new Creature("vampires.nosferatu", Attack(5), 34, I18n("vampires.nosferatu.description"), reaction = new NosferatuReaction),

    new Creature("vampires.aristocrat", Attack(7), 36, I18n("vampires.aristocrat.description"), runAttack = new AristoAttack, effects = effects(OnTurn -> aristo)),

    new Creature("vampires.mansion", Attack(0), 40, I18n("vampires.mansion.description"), reaction = new MansionReaction, effects = effects(Direct -> ghoulify))), eventListener = Some(new CustomListener(new VampireEventListener)))

  val ghoul = Vampire.cards(2).asCreature
  val aristocrat = Vampire.cards(6).asCreature
  Vampire initCards Houses.basicCostFunc

  val neophyte = new Creature("vampires.neophyte", Attack(5), 10, I18n("vampires.neophyte.description"), runAttack = new NeophyteAttack)

  neophyte.houseIndex = Vampire.houseIndex
  neophyte.houseId = Vampire.houseId

  private def darkFlock = { env: Env ⇒
    import env._
    (player.slots reduce lowestLife) foreach { s ⇒
      val card = s.get.card
      player.slots.move(s.num, selected)
      if (card.cost < 10) {
        val slot = player.slots(selected)
        slot toggle CardSpec.invincibleFlag
        player addEffectOnce (OnTurn -> RemoveInvincible(slot.get.id))
      }
    }
  }

  private def bloodTies = { env: Env ⇒
    import env._
    val slot = getOwnerSelectedSlot()
    val attack = slot.get.attack
    slot.destroy()
    slot.filledAdjacents foreach { s ⇒
      val card = s.get.card
      if (card.cost < 10 && card.runAttack != MultiTargetAttack) {
        val bonus = AttackAdd(attack)
        s.attack add bonus
      }
    }
	player.houses.incrMana(3, 4)
  }

  private def aristo = { env: Env ⇒
    import env._
    val slots = player.slots
    (otherPlayer.slots reduce lowestLife) foreach { s ⇒
      if (s.num != selected && !slots(s.num).value.exists(_.card == aristocrat)) {
        slots.move(env.selected, s.num)
      }
    }
  }

  private def ghoulify: Effect = { env: Env ⇒
    import env._
    getOwnerSelectedSlot().adjacentSlots foreach { slot ⇒
      if (slot.value.isDefined) {
        slot.destroy()
        slot add ghoul
      }
    }
  }

  class MansionReaction extends Reaction {
    final override def onDeath(dead: Dead) {
      import dead._
      if (selected.playerId == player.id && card.houseIndex < 4) {
        player.slots(num) add neophyte
      }
    }
  }

  class AcolyteReaction extends Reaction {
    def onDamaged(slot : SlotUpdate, d: Damage, p: PlayerUpdate) = {
      if (slot.value.isDefined && d.amount > 5) {
        slot inflict Damage(4, Context(selected.playerId, Some(acolyte), selected.num), isAbility = true)
		
		// приносит хозяину 1 ману его стихии.
		//slot.value foreach { s =>
		//	p.houses.incrMana(1, s.card.houseIndex)
        //}
        true
      } else false
    }
  }

  class VampireEventListener extends HouseEventListener with AnyDeathEventListener {
    // broadcast enemy damage
    def onDamaged(card: Creature, d: Damage, slot: SlotUpdate, p: PlayerUpdate) {
      player.slots foreach { s ⇒
        s.get.reaction match {
          case r : AcolyteReaction if r.onDamaged(slot, d, p) => s.focus(blocking = false)
          case _ =>
        }
      }
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.slots.slots foreach { slot =>
        slot.onDamage = (FuncDecorators decorate slot.onDamage) after { case (slotState, d) =>
          onDamaged(slotState.card, d, slot, p)
        }
      }
    }
  }
}

private class NoctuleAttack extends RunAttack {
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒
        val oldl = otherPlayer.value.life
        otherPlayer inflict d
        player.heal(oldl - otherPlayer.value.life)
      case Some(_) ⇒
        slot inflict d
    }
  }
}

private class GhoulAttack extends RunAttack {
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒
        otherPlayer inflict d
      case Some(slotState) ⇒
        if (slotState.life < 11) {
          slot inflict Damage(1000, d.context)
        } else slot inflict d
    }
  }
}

class NosferatuReaction extends Reaction {
  final override def onDeath(dead: Dead) {
    selected heal 2
    selected.player heal 3
  }
}


class AristoAttack extends RunAttack {
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒
        otherPlayer inflict d
      case Some(_) ⇒
        val attack = slot.get.attack
        slot inflict d
        if (slot.value.isEmpty) {
          otherPlayer inflict Damage(attack, d.context, isAbility = true)
        }
    }
  }
}

class NeophyteAttack extends RunAttack with DamageAttack {

  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val healAmount = damageAndGet(num, d, player)
    player.slots(num) heal math.ceil(healAmount / 2f).toInt
  }
}
