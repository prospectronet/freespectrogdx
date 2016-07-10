package priv.sp.house

import priv.sp._
import priv.sp.update._
import GameCardEffect._
import priv.util.FuncDecorators

object Entomologist extends ChangeTarget {
  import CardSpec._

  val giantAnt = new Creature("entomologist.ant", Attack(5), 23, I18n("entomologist.ant.description"), reaction = new GiantReaction, effects = effects(Direct -> ant))
  val assassinWasp = new Creature("entomologist.assassin", AttackSources(Some(5), Vector(AssassinAttackSource)), 26, I18n("entomologist.assassin.description"), effects = effects(Direct -> assassin))

  val Entomologist: House = House("entomologist", List(
    new Creature("entomologist.beetle", Attack(3), 12, I18n("entomologist.beetle.description"),
      reaction = new FireBeetleReaction,
      effects = effects(Direct -> beetle)),
    new Creature("entomologist.moth", Attack(3), 14, I18n("entomologist.moth.description"), effects = effects(OnTurn -> damage(1, isAbility = true), OnTurn -> focus(damageCreatures(1, isAbility = true))), reaction = new MothReaction),
    //Spell("entomologist.hivemind", I18n("entomologist.hivemind.description"), inputSpec = Some(SelectTargetCreature), effects = effects(Direct -> hivemind)),
	Spell("entomologist.hivemind", I18n("entomologist.hivemind.description"), effects = effects(Direct -> hivemindHeal)),
    Spell("entomologist.locust", I18n("entomologist.locust.description"), inputSpec = Some(SelectTargetCreature), effects = effects(Direct -> locust)),
    giantAnt,
    assassinWasp,
    new Creature("entomologist.hive", Attack(2), 30, I18n("entomologist.hive.description"), effects = effects(OnEndTurn -> hive)),
    new Creature("entomologist.mantis", Attack(7), 47, I18n("entomologist.mantis.description"), effects = effects(Direct -> { env: Env ⇒
      env.otherPlayer.houses.incrMana(-1, 0, 1, 2, 3, 4)
    }), reaction = new MantisReaction)),
    data = Targeting(),
    eventListener = Some(new CustomListener(new EntoEventListener)))

  Entomologist.initCards(Houses.basicCostFunc)

  private val drone = new Creature("entomologist.drone", Attack(2), 14, reaction = new DroneReaction)
  private val insect = new Creature("entomologist.warrior", Attack(4), 10, I18n("entomologist.warrior.description"), reaction = new InsectReaction)

  drone.cost = 0
  insect.cost = 1

  List(drone, insect).foreach { c ⇒
    c.houseIndex = Entomologist.houseIndex
    c.houseId = Entomologist.houseId
  }

  def beetle: Effect = { env: Env ⇒
    val selected = env.getOwnerSelectedSlot()
    selected.oppositeSlot inflict Damage(4, env, isAbility = true)
  }

  class FireBeetleReaction extends Reaction {
    override def onMyDeath(dead: Dead) {
      selected.focus()
      selected.player.houses.incrMana(1, 0)
    }
  }

  class MothReaction extends Reaction {
    override def onMyDeath(dead: Dead) {
      selected.focus()
      selected.otherPlayer.slots inflictCreatures Damage(3, Context(selected.playerId, Some(dead.slot.card), dead.num), isAbility = true)
    }
  }

  def hivemind = changeTarget
  
  def hivemindHeal = { env: Env ⇒
    import env._
    val ownMaxMana = getOwnMaxMana(player.getHouses)
    player.slots healCreatures (ownMaxMana)
  }

  
  def getOwnMaxMana(houses : PlayerState.HousesType) = {
	val own = houses.reduceLeft((h1, h2) ⇒ if (h1.mana > h2.mana) h1 else h2).mana
	math.max(0, own)
  }


  def locust = { env: Env ⇒
    import env._
    val s = getTargetSelectedSlot()
    s toggle CardSpec.cursedFlag
    player addEffectOnce (OnEndTurn -> Locust(s.get.id))
  }

  def ant = { env: Env ⇒
    env.player addDescMod LowerGiantCostMod
  }

  class GiantReaction extends Reaction {
    override def selfProtect(d: Damage) = {
	  if (!d.isSpell) {
		d.copy(amount = math.max(0, d.amount - 2))
	  }
	  else
		d
    }
    override def cleanUp() {
      selected.player removeDescMod LowerGiantCostMod
    }
  }

  case object LowerGiantCostMod extends DescMod {
    def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
      if (house.houseIndex < 4) cards
      else cards.map { c ⇒
        if (c.card == giantAnt) {
          c.copy(cost = math.max(0, c.cost - 2))
        } else c
      }
    }
  }

  private def assassin = { env: Env ⇒
    def spawnDroneAt(num: Int) {
      if (env.player.value.isInSlotRange(num)) {
        val slot = env.player.slots(num)
        if (slot.value.isEmpty) {
          slot add drone
        }
      }
    }
    spawnDroneAt(env.selected - 1)
    spawnDroneAt(env.selected + 1)
  }

  case object AssassinAttackSource extends AttackStateFunc {
    def apply(attack: Int, player: PlayerUpdate) = {
      val nbDrone = player.slots.slots.count { s ⇒
        s.value.isDefined && s.get.card == drone
      }
      attack + nbDrone * 2
    }
  }

  class DroneReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) { setAssassinDirty(selected.slots) }
    override def onMyRemove(dead: Option[Dead]) { setAssassinDirty(selected.slots) }
    def setAssassinDirty(slots: SlotsUpdate) {
      slots foreach { s ⇒
        if (s.get.card == assassinWasp) {
          s.attack.setDirty()
        }
      }
    }
  }

  def hive: Effect = { env: Env ⇒
    val openSlots = env.player.slots.getOpenSlots
    if (openSlots.nonEmpty) {
      val slot = openSlots(scala.util.Random.nextInt(openSlots.size))
      slot add insect
      slot.focus(blocking = false)
    }
	effects(Direct -> damage(5, isSpell = true))
  }

  class InsectReaction extends Reaction {
    override def onMyDeath(dead: Dead) {
      selected.player.houses.incrMana(1, 4)
    }
  }

  class MantisReaction extends Reaction {
    override def onAdd(slot: SlotUpdate) : Unit = {
      if (slot.num == selected.num) {
        selected.oppositeSlot.value foreach { s =>
          bridle(s.card)
        }
      }
    }
    override def onMyRemove(dead: Option[Dead]): Unit = {
      unbridle()
    }
    def onOppAdd(s : SlotState, num : Int) : Unit = {
      if (num == selected.num) {
        unbridle()
        bridle(s.card)
      }
    }

    def onOppRemove(deadOpt : Option[Dead], num : Int) : Unit = {
      if (num == selected.num) {
        unbridle()
      }
    }

    def bridle(oppCard : Card) : Unit = {
      selected.otherPlayer.houses.incrGrowth(-2, oppCard.houseIndex)
      selected setData oppCard
    }

    def unbridle() : Unit = {
      selected.value flatMap (s => Option(s.data)) foreach { data =>
        val card = data.asInstanceOf[Card]
        selected.otherPlayer.houses.incrGrowth(2, card.houseIndex)
        selected.setData(null)
      }
    }

    override def onMyDeath(dead: Dead) {
      selected.oppositeSlot inflict Damage(5, Context(selected.playerId, Some(dead.slot.card), dead.num), isAbility = true)
      unbridle()
    }
  }

  def getTargeting(player : PlayerUpdate) : Targeting = player.value.data.asInstanceOf[Targeting]
  def setTarget(player : PlayerUpdate, target : Option[Int] ) : Unit = player.updateData[Targeting](x ⇒ x.copy(target = target))

  class EntoEventListener extends ChangeTargetListener with OppDeathEventListener {
    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.slots.slots foreach { s =>
        s.add = (FuncDecorators decorate s.add) after (slotState => reactOppnentAdd(slotState, s.num))
        s.remove = (FuncDecorators decorate s.remove) after (deadOpt => reactOppnentRemove(deadOpt, s.num))
      }
    }

    def reactOppnentAdd(slotState : SlotState, num : Int) = {
      player.slots.filleds foreach { s ⇒
        s.value.get.reaction match {
          case r : MantisReaction => r.onOppAdd(slotState, num)
          case _ =>
        }
      }
    }

    def reactOppnentRemove(deadOpt : Option[Dead], num : Int) = {
      player.slots.filleds foreach { s ⇒
        s.value.get.reaction match {
          case r : MantisReaction => r.onOppRemove(deadOpt, num)
          case _ =>
        }
      }
    }
  }
}

case class Locust(id: Int) extends Function[Env, Unit] {
  def apply(env: Env) {
    (env.otherPlayer.slots findSlot id) match {
      case Some(s) ⇒
        val damage = Damage(8, Context(env.playerId, None), isSpell = true)
        val playerDamage = damage.copy(amount = 4)
        env.otherPlayer inflict playerDamage
        s inflict damage
      case None ⇒ env.player removeEffect (_ == this)
    }
  }
}

