package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update._
import priv.util.FuncDecorators

import scala.util._

class FairyKnight {

  val vodyanoy = new Creature("fairy.vodyanoy", Attack(4), 11)
  val rusalka = new Creature("fairy.rusalka", Attack(6), 19,
                             I18n("fairy.rusalka.description"),
                             reaction = new RusalkaReaction)
  val likho = new Creature("fairy.likho", Attack(6), 21 ,
    I18n("fairy.likho.description"), reaction = new LikhoReaction)
  val gory = new Creature("fairy.zmey", Attack(9), 35,
    I18n("fairy.zmey.description"), reaction = new ZGoryReaction)
  val someGory = Some(gory)
  val thgorynych = new Creature("fairy.2head", Attack(6), 25, reaction = new THGoryReaction)
  val ohgorynych = new Creature("fairy.1head", Attack(3), 15, reaction = new OHGoryReaction)

  val Fairy = House("fairy", List(
    new Creature("fairy.lesovik", Attack(4), 18,
                 I18n("fairy.lesovik.description"),
                 effects = effects(Direct -> lesovik), reaction = new LesovikReaction),
    new Creature("fairy.kikimora", Attack(3), 13,
                 I18n("fairy.kikimora.description"), 
                 effects = effects(Direct -> kiki)),
    rusalka,
    likho,
    new Creature("fairy.sirin", Attack(4), 23,
                 I18n("fairy.sirin.description"),
                 effects = effects(Direct -> sirin), reaction = new SirinReaction),
    new Creature("fairy.alkonost", Attack(6), 31,
      I18n("fairy.alkonost.description"),
      effects = effects(Direct -> alko, OnTurn -> alko),
      reaction = new AlkoReaction),
    new Creature("fairy.firebird", Attack(8), 32,
                 I18n("fairy.firebird.description"), reaction = new FirebirdReaction),
    gory),
    eventListener = Some(new CustomListener(new FKEventListener)))

  Fairy initCards Houses.basicCostFunc
  Fairy.addAdditionalCards(vodyanoy, thgorynych, ohgorynych)
  vodyanoy.cost = 1
  thgorynych.cost = 8
  ohgorynych.cost = 8

  trait OnOppSlotUpdate {
    def onOppAdd(slot: SlotUpdate)
    def onOppRemove(slot: SlotUpdate)
  }

  class LesovikReaction extends Reaction with OnOppSlotUpdate {
    final def onOppAdd(slot: SlotUpdate) = {
      if (slot.num == selected.num) {
        addLesovikMod(selected, slot)
      }
    }

    final def onOppRemove(slot: SlotUpdate) {
      if (slot.num == selected.num) {
        removeMod()
      }
    }
    override def cleanUp() { removeMod() }

    def removeMod() {
      val data = selected.get.data
      if (data != null) {
        selected.otherPlayer removeDescMod data.asInstanceOf[Destroyed]
      }
    }
  }

  def addLesovikMod(selected: SlotUpdate, slot: SlotUpdate) {
    slot.value.foreach { s ⇒
      val destroyed = Destroyed(s.card)
      slot.player addDescMod destroyed
      selected setData destroyed
    }
  }

  def lesovik = { env: Env ⇒
    import env._
    val slot = getOwnerSelectedSlot
    val oppSlot = slot.oppositeSlot
    addLesovikMod(slot, oppSlot)
  }

  def kiki = { env: Env ⇒
    import env._
    val openSlots = player.slots.getOpenSlots
    if (openSlots.nonEmpty) {
      val slot = openSlots(updater.randLogs.get(openSlots.size))
      slot add vodyanoy
      slot.focus(blocking = false)
    }
  }

  val someRusalka = Some(rusalka)
  class RusalkaReaction extends Reaction {
    final override def onDeath(dead: Dead) {
      if (dead.player.id != selected.playerId && dead.num == selected.num) {
        dead.damage foreach { d ⇒
          if (d.context.card == someRusalka) {
            val destroyed = Destroyed(dead.card)
            dead.player addDescMod destroyed
          }
        }
      }
    }
  }

  val someLikho = Some(likho)
  class LikhoReaction extends Reaction {
    lazy val context = Context(selected.playerId, someLikho, selected.num)

    override def onMyDeath(dead: Dead) {
      dead.damage match {
        case Some(d) if (d.context.selected != Context.noSelection) && !d.isEffect ⇒
          val oppSlot = selected.player.otherPlayer.slots(d.context.selected)
          oppSlot.value foreach { s ⇒
            val l = s.life
            oppSlot drain Damage(if (l < 11) l else (l - 10), context, isAbility = true)
          }
        case _ ⇒ selected.player.houses.incrMana(2, selected.updater.randLogs.get(5))
      }
    }
  }

  def sirin = { env: Env ⇒
    import env._
    val malus = SirinMalus(selected)
    val slot = getOwnerSelectedSlot
    val oppSlot = slot.oppositeSlot
    if (oppSlot.value.isDefined) {
      malus temper oppSlot
    }
    oppSlot.filledAdjacents foreach malus.temper
  }

  class SirinReaction extends Reaction with OnOppSlotUpdate {
    lazy val malus = SirinMalus(selected.num)

    def isInRange(s: SlotUpdate) = math.abs(s.num - selected.num) < 2
    final def onOppAdd(slot: SlotUpdate) = if (isInRange(slot)) { malus temper slot }
    final def onOppRemove(slot: SlotUpdate) = if (isInRange(slot)) { malus untemper slot }
    override def cleanUp() {
      val oppSlot = selected.oppositeSlot
      if (oppSlot.value.isDefined) {
        malus untemper oppSlot
      }
      oppSlot.filledAdjacents foreach malus.untemper
    }
  }

  def alko = { env: Env ⇒
    import env._
    val slot = getOwnerSelectedSlot
    val cards = otherPlayer.value.desc.get.houses flatMap (_.cards.map(_.card))
    val filtereds = slot.value match {
      case Some(x) if x.data != null ⇒
        val olds = x.data.asInstanceOf[Destroyeds]
        otherPlayer removeDescMod olds
        cards filterNot olds.excls.contains
      case _ ⇒ cards
    }
    val excls = updater.randLogs.unorderedShuffle(filtereds, 6).to[Set]
    val destroyeds = Destroyeds(excls)
    otherPlayer addDescMod destroyeds
    slot setData destroyeds
  }

  class AlkoReaction extends Reaction {
    override def cleanUp() {
      selected.value match {
        case Some(x) if x.data != null ⇒
          selected.otherPlayer removeDescMod x.data.asInstanceOf[Destroyeds]
        case _ ⇒
      }
    }
  }

  class FirebirdReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      val adjs = adjacents(selected.num) flatMap (x ⇒ selected.player.value.slots.get(x))
      if (adjs.exists(_.card.houseIndex == 0) || selected.filledAdjacents.exists(_.get.card.houseIndex == 0)) {
        d.copy(amount = 0)
      } else d
    }
  }

  trait GoryReaction extends Reaction {
    lazy val context = Context(selected.playerId, someGory, selected.num)

    override def onSummon(summoned: SummonEvent) {
      if (summoned.card.houseIndex == 0) {
        val ss = summoned.player.otherPlayer.slots.filleds
        if (ss.nonEmpty) {
          ss(Random.nextInt(ss.size)) inflict Damage(6, context, isAbility = true)
        }
      }
    }
  }
  class ZGoryReaction extends GoryReaction {
    override def destroy() { selected add thgorynych }
    override def onMyDeath(dead: Dead) { selected add thgorynych }
  }

  class THGoryReaction extends GoryReaction {
    override def destroy() { selected add ohgorynych }
    override def onMyDeath(dead: Dead) { selected add ohgorynych }
  }

  class OHGoryReaction extends GoryReaction

  class FKEventListener extends HouseEventListener with OppDeathEventListener {
    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.slots.slots foreach { slot ⇒
        slot.add = (FuncDecorators decorate slot.add) after { _ ⇒
          player.slots foreach { s ⇒
            s.get.reaction match {
              case r: OnOppSlotUpdate ⇒ r onOppAdd slot
              case _                  ⇒
            }
          }
        }
        slot.remove = (FuncDecorators decorate slot.remove) before { _ ⇒
          player.slots foreach { s ⇒
            s.get.reaction match {
              case r: OnOppSlotUpdate ⇒ r onOppRemove slot
              case _                  ⇒
            }
          }
        }
      }
    }
  }
}

case class SirinMalus(id: Int) extends AttackFunc {
  def apply(attack: Int) = math.max(0, math.ceil(attack / 2f).intValue)
  def temper(s: SlotUpdate) { s.attack add this }
  def untemper(s: SlotUpdate) { s.attack removeFirst this }
}
