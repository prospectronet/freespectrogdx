package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._

class Dreamweaver {

  val roc = new Creature("dreamweaver.roc", AttackSources(Some(6), Vector(RocAttackSource)), 17,
    "dreamweaver.roc.description")

  val castle = new Creature("dreamweaver.castle", Attack(2), 37,
    I18n("dreamweaver.castle.description"),
    runAttack = MultiTargetAttack, reaction = new CastleReaction)

  val Dreamweaver = House("Dreamweaver", List(
    new Creature("dreamweaver.wisp", AttackSources(Some(2), Vector(EtherealAttackSource)), 8,
      I18n("dreamweaver.wisp.description"),
      reaction = new EtherealReaction),

    roc,

    Spell("dreamweaver.aurora", I18n("dreamweaver.aurora.description"),
      effects = effects(Direct -> aurora)),

    new Creature("dreamweaver.guide", Attack(4), 19,
      I18n("dreamweaver.guide.description"),
      effects = effects(Direct -> guide)),

    new Creature("dreamweaver.sword", AttackSources(Some(4), Vector(SwordAttackSource)), 28,
      I18n("dreamweaver.sword.description"),
      reaction = new SwordReaction, runAttack = new SwordAttack),

    new Creature("dreamweaver.butterfly", Attack(4), 31,
      I18n("dreamweaver.butterfly.description"),
      reaction = new RainbowReaction),

    castle,

    new Creature("dreamweaver.mare", AttackSources(Some(5), Vector(EtherealAttackSource)), 44,
      I18n("dreamweaver.mare.description"),
      effects = effects(Direct -> mare), reaction = new NightmareReaction)),
    eventListener = Some(new CustomListener(new DreamweaverEventListener)))

  Dreamweaver.initCards(Houses.basicCostFunc)

  def aurora = { env: Env ⇒
    import env._
    def aur(p: PlayerUpdate) = p.slots.foreach { s ⇒
      val card = s.get.card
      s.heal(12)
      p.houses.incrMana(1, card.houseIndex)
    }

    aur(player)
    aur(otherPlayer)
  }

  def guide = { env: Env ⇒
    import env._
    val nbCreatures = player.getSlots.size
    val dp = getMana(4)
    player.slots foreach { s ⇒
      if (s.num != selected) { // bs? apply effect before interception!?
        s heal dp
      }
    }
    player heal (nbCreatures * 3)
  }

  def mare = { env: Env ⇒
    import env._
    val damage = Damage(6 * otherPlayer.slots.slots.count(_.value.isEmpty), env, isAbility = true)
    otherPlayer.slots inflictCreatures damage
  }

  class EtherealReaction extends NightmareReaction {
    override def selfProtect(d: Damage) = {
      if (d.isEffect && d.context.playerId != selected.playerId) d.copy(amount = 0) else d
    }
  }

  class NightmareReaction extends Reaction {
    override def onAdd(slot: SlotUpdate) = {
      selected.attack.setDirty()
    }
    override def onDeath(dead: Dead) {
      import dead._
      selected.attack.setDirty()
    }
  }

  private class SwordReaction extends Reaction {
    override def onAdd(slot: SlotUpdate) = if (math.abs(selected.num - slot.num) == 1) selected.attack.setDirty()
    override def onDeath(dead: Dead) {
      if (math.abs(selected.num - dead.num) == 1) selected.attack.setDirty()
    }
  }

  private class RainbowReaction extends Reaction {

    override def onSummon(summoned: SummonEvent) {
      import summoned._
      if (selected.playerId != player.id) {
        val p = player.otherPlayer
        p.houses.incrMana(3, card.houseIndex)
      }
    }
  }

  class DreamweaverEventListener extends HouseEventListener with OwnerDeathEventListener {
    def refreshRoc() {
      if (player.getSlots.values exists (_.card == roc)) {
        player.slots.filleds.withFilter(_.get.card == roc) foreach { s ⇒
          s.attack.setDirty()
        }
      }
    }
    def protect(slot: SlotUpdate, damage: Damage) = {
      player.slots.foldl(damage) { (acc, s) ⇒
        val c = s.get.card
        if (c == castle) {
          s.get.reaction onProtect DamageEvent(acc, Some(slot.num), player)
        } else acc
      }
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.slots.slots foreach { slot ⇒
        slot.protect modifyResult (d ⇒ protect(slot, d))
      }
      p.otherPlayer.slots.update after { _ ⇒ refreshRoc() }
    }
  }
}

case object EtherealAttackSource extends AttackStateFunc {
  def apply(attack: Int, player: PlayerUpdate) = {
    val nbCreatures = player.slots.filleds.size
    attack + nbCreatures - 1
  }
}

case object RocAttackSource extends AttackSlotStateFunc {
  def apply(attack: Int, slot: SlotUpdate) = {
    if (slot.otherPlayer.getSlots isDefinedAt slot.num) {
      attack
    } else {
      attack / 2
    }
  }
}

case object SwordAttackSource extends AttackSlotStateFunc {
  def apply(attack: Int, slot: SlotUpdate) = {
    val nbAdjacents = slot.adjacentSlots count (_.value.isDefined)
    attack + nbAdjacents
  }
}

class SwordAttack extends RunAttack {
  isMultiTarget = true

  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    val oppSlot = otherPlayer.slots(num)
    val targets = oppSlot :: oppSlot.adjacentSlots

    targets.foreach { slot ⇒
      if (slot.num == num && slot.value.isEmpty) {
        player.otherPlayer inflict d
      }

      if (slot.value.isDefined) {
        slot inflict d
      }
    }
  }
}

class CastleReaction extends Reaction {
  final override def onProtect(d: DamageEvent) = {
    if (d.target.isDefined && d.target != Some(selected.num)) {
      d.damage.copy(amount = math.max(0, d.damage.amount - 2))
    } else d.damage
  }
}

