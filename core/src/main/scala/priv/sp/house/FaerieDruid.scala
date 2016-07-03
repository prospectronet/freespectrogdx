package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update.{PlayerUpdate, HouseEventListener, SlotUpdate}
import priv.util.FuncDecorators

object FaerieDruid {
  val rabble = new Creature("faerie.rabble", Attack(2), 10,
    I18n("faerie.rabble.description"),
  reaction = new RabbleReaction)

  val FaerieDruid : House = House("faerie", List(

    new Creature("faerie.twins", Attack(4), 12,
      I18n("faerie.twins.description"),
      effects = effects(Direct -> twinHeal),
      reaction = new TwinReaction),

    Spell("faerie.fire", I18n("faerie.fire.description"),
      effects = effects(Direct -> fire)),

    new Creature("faerie.nightwing", Attack(4), 18,
      I18n("faerie.nightwing.description"),
      reaction = new NightwingReaction),

    new Creature("faerie.ring", Attack(2), 20,
      I18n("faerie.ring.description"),
      effects = effects(OnTurn -> addMana(1, 4)),
      reaction = new RingReaction),

    new Creature("faerie.piper", Attack(4), 28,
      I18n("faerie.piper.description"),
    effects = effects(OnTurn -> piper)),

    Spell("faerie.cloud", I18n("faerie.cloud.description"),
      effects = effects(Direct -> cloud)),

    new Creature("faerie.unicorn", Attack(7), 34,
      I18n("faerie.unicorn.description"),
      status = runFlag,
      reaction = new UnicornReaction),

    new Creature("faerie.dragon", Attack(9), 39,
      I18n("faerie.dragon.description"),
    effects = effects(Direct -> dragon))),

    eventListener = Some(new CustomListener(new FaerieDruitListener)))

  rabble.cost = 1
  FaerieDruid initCards Houses.basicCostFunc
  FaerieDruid.addAdditionalCards(rabble)

  def fire = { env : Env =>
    import env._
    val d = Damage(5, env, isSpell = true)
    otherPlayer.slots foreach { s =>
      if (player.slots.slots(s.num).value.isDefined) s inflict d else s.stun()
    }
  }

  def cloud = GameCardEffect fillEmptySlots rabble

  def twinHeal = { env : Env => env.player.slots(env.selected).adjacentSlots foreach (_ heal 4) }

  def piper = { env : Env =>
    import env._
    player.slots(selected).adjacentSlots foreach (_ heal 3)
    focus()
  }

  def dragon = { env : Env =>
    import env._
    val bonus = AttackAdd(2)
    player.slots foreach { s =>
      s heal 20
      if (s.num != selected) {
        s.attack add bonus
      }
    }
    player addEffectOnce (OnEndTurn -> new RemoveAttack(bonus))
  }

  class NightwingReaction extends Reaction {
    override def onAdd(slot : SlotUpdate): Unit = {
      if (slot == selected) {
        val malus = Lower1Attack(selected.num)
        selected.otherPlayer.slots foreach malus.temper
      }
    }

    def onEnemyAdd(slot: SlotUpdate) = {
      Lower1Attack(selected.num) temper slot
    }
    final override def onRemove(slot: SlotUpdate) = {
      if (slot != selected && slot.playerId != selected.playerId) {
        val malus = Lower1Attack(selected.num)
        slot.attack removeFirst malus
      }
    }
    final override def cleanUp(): Unit = {
      val malus = Lower1Attack(selected.num)
      def removeMalus(s: SlotUpdate) { s.attack removeFirst malus }
      selected.otherPlayer.slots foreach removeMalus
    }
  }

  class RingReaction extends Reaction {

    final def onSubmit(c : Command) = {
      if (c.card.houseIndex == 4) {
        val bonus = AttackAdd(2)
        selected.attack add bonus
        selected.player addEffectOnce (OnEndTurn -> new RemoveAttack(bonus))
      }
    }
  }

  class TwinReaction extends Reaction {
    override def onMyDeath(dead : Dead) = selected.adjacentSlots foreach (_ heal 4)
  }

  class RabbleReaction extends Reaction {
    override def onMyDeath(dead : Dead) = selected.player heal 4
  }


  class UnicornReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) = {
      if (selected.num == slot.num) {
        selected.player addDescMod LowerCreatureCostMod
      }
    }
    final override def onMyRemove(dead: Option[Dead]) = {
      selected.player removeDescMod LowerCreatureCostMod
    }
  }

  class FaerieDruitListener extends HouseEventListener {

    def onEnemyAdd(slot: SlotUpdate): Unit = {
      player.slots foreach { s ⇒
        s.get.reaction match {
          case n: NightwingReaction ⇒ n onEnemyAdd slot
          case _                 ⇒ ()
        }
      }
    }

    final override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.slots.slots foreach { slot ⇒
        slot.add = (FuncDecorators decorate slot.add) after { _ ⇒ onEnemyAdd(slot) }
      }
      p.submitCommand = (FuncDecorators decorate p.submitCommand) after { c ⇒
        player.slots foreach { s ⇒
          s.get.reaction match {
            case r: RingReaction ⇒ r onSubmit c
            case _ ⇒
          }
        }
      }
    }
  }
}

case object LowerCreatureCostMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards map {  c ⇒
      if (!c.card.isSpell) {
        c.copy(cost = math.max(c.cost - 1, 0))
      } else c
    }
  }
}
