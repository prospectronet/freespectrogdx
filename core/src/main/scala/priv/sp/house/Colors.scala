package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect.Env
import priv.sp._
import priv.sp.update.{SlotUpdate, PlayerUpdate, HouseEventListener}
import priv.util.FuncDecorators

object Colors {

  // /!\ direct effect should use player data because slot data is not set yet
  val violet = new Creature("colors.Violet.name", AttackSources(), 29,
    I18n("colors.Violet.description"),
	effects = effects(Direct -> violetEffect),
	reaction = new VioletReaction)

  val azure = new Creature("colors.Azure.name", AttackSources(), 24,
    I18n("colors.Azure.description"),
    effects = effects(OnTurn -> azureEffect),
    reaction = new AzureReaction)

  val Colors: House = House("colors", List(
    Spell("colors.Discoloration.name", 
	  I18n("colors.Discoloration.description"),
      inputSpec = Some(SelectOwner(selectColour)),
      effects = effects(Direct -> discolor)),

    new Creature("colors.Amber.name", AttackSources(), 21,
      I18n("colors.Amber.description"),
      reaction = new AmberReaction),

    violet,

    azure,

    new Creature("colors.Silver.name", AttackSources(), 33,
      I18n("colors.Silver.description"),
      effects = effects(OnTurn -> silver)),

    new Creature("colors.Gold.name", Attack(0), 35,
      I18n("colors.Gold.description"),
      reaction = new GoldReaction),

    new Creature("colors.Emerald.name", AttackSources(), 16,
      I18n("colors.Emerald.description"),
      reaction = new EmeraldReaction),

    new Creature("colors.Crimson.name", AttackSources(), 34,
      I18n("colors.Crimson.description"),
      reaction = new CrimsonReaction)),

  eventListener = Some(new CustomListener(new ColorListener)),
  description =  I18n("colors.description"))

  Colors.initCards(Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 2, 4 -> 3, 5 -> 4, 6 -> 4, 7 -> 4))

  class CrimsonReaction extends Reaction {

    override def onProtect(d: DamageEvent) = {
      if (d.target.isEmpty
        && ! d.damage.isEffect
        && ! d.damage.context.card.exists(_.asCreature.runAttack.isMultiTarget)) {
        d.damage.copy(amount = d.damage.amount + math.max(0, getData(selected) -3))
      } else d.damage
    }
  }

  class EmeraldReaction extends Reaction {

    final override def onAdd(slot: SlotUpdate) = {
      if (selected.num == slot.num) {
        // slot data is not set yet !!
        selected.player addDescMod IncrSpecialCostMod(getData(slot.player))
      }
    }

    final override def onMyRemove(dead: Option[Dead]) = {
      selected.player removeDescMod IncrSpecialCostMod(getData(selected))
    }

    override def onProtect(d: DamageEvent) = {
      if (d.target.isEmpty) {
        val x = getData(selected)
        d.damage.copy(amount = math.max(0, d.damage.amount - math.max(0, x)))
      } else d.damage
    }

  }

  case class IncrSpecialCostMod(x : Int) extends DescMod {
    def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
      if (house.houseIndex == 4) cards.map(c ⇒ c.copy(cost = c.cost + x))
      else cards
    }
  }

  val goldBonus = AttackAdd(1)
  class GoldReaction extends Reaction {

    override def onDeath(dead : Dead) = {
      if (dead.num == selected.num -1) {
        val x = getData(selected)
        val slot = selected.slots(dead.num)
        val slotState = selected.slots.buildSlotState(slot, dead.card)
        slot add slotState.copy(life = math.min(4 * x, slotState.life))
        selected.destroy()
        selected.otherPlayer.slots foreach { _.attack add goldBonus }
      }
    }
  }

  def silver = { env : Env =>
    val x = getData(env.getOwnerSelectedSlot())
    env.player.slots.healCreatures(x - 1)
    if (x > 5) {
      env.player.houses.incrMana(-1, 4)
    }
  }

  class AzureReaction extends Reaction {
    val someAzure = Some(azure)

    final override def onProtect(d: DamageEvent) = {
      if (d.damage.isEffect && d.damage.context.card != someAzure && d.damage.context.playerId != selected.playerId) {
        val x = getData(selected)
        (for {
          i <- d.target
          slot = selected.slots(i)
          s <- slot.value
          if s.card.cost > x -2 && s.card.cost < x + 2
        } yield d.damage.copy(amount = 0)) getOrElse d.damage
      } else d.damage
    }
  }

  def azureEffect = { env : Env =>
    val x = getData(env.getOwnerSelectedSlot())
    val damage = Damage(3, env, isAbility = true)
    env.player.slots foreach { slot =>
      if (slot.get.card.cost > x -2 && slot.get.card.cost < x + 2) {
        slot inflict damage
      }
    }
  }

  def violetEffect = { env : Env =>
    env.player addDescMod Lower2XCostMod(getData(env.player))
  }

  class VioletReaction extends Reaction {

    final override def onProtect(d: DamageEvent) = {
      if (d.damage.isSpell && d.damage.context.playerId != selected.playerId) {
        d.damage.copy(amount = math.max(0, d.damage.amount - 3))
      } else d.damage
    }
  }

  class AmberReaction extends Reaction {

    override def onMyDeath(dead: Dead) {
      val x = getData(dead.slot)
      selected.focus()
      selected.player.getHouses.zipWithIndex.sortBy(_._1.mana).takeRight(2) foreach { case (_, idx) =>
        selected.player.houses.incrMana(x, idx)
      }
    }
  }

  class ColorListener extends HouseEventListener with OwnerDeathEventListener {

    def protect(slot: SlotUpdate, damage: Damage) = {
      player.slots.foldl(damage) { (acc, s) ⇒
        val c = s.get.card
        if (c == violet || c == azure) {
          s.get.reaction onProtect DamageEvent(acc, Some(slot.num), player)
        } else acc
      }
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.slots.slots foreach { slot ⇒
        slot.protect modifyResult (d ⇒ protect(slot, d))
      }
      p.submitCommand = (FuncDecorators decorate p.submitCommand)
        .before { _ => p.setData(ColorData(p.pstate.houses(4).mana)) }
        .after { c =>
        if (c.card.houseIndex == 4) {
          player addDescMod HideCardMod(c.card)
          c.card match {
            case creature: Creature if creature.attack.base != some0 =>
              c.input foreach { i =>
                val slot = p.slots(i.num)
                if (slot.value.isDefined) {
                  val x = getData(p)
                  p.houses.incrMana(-x, 4) // zeroize
                  slot.attack add AttackAdd(x)
                  slot setData ColorData(x)
                }
              }
            case _ =>
          }
        }
      }
      p.slots.onDead = (FuncDecorators decorate p.slots.onDead) after { dead ⇒
        if (dead.card.houseIndex == 4) {
          player removeDescMod HideCardMod(dead.card)
        }
      }
      // ice guard should be applied after crimson
      p.otherPlayer.guard = (FuncDecorators decorate p.otherPlayer.guard) update { f =>
        { damage =>
          f(player.slots.foldl(damage) { (acc, s) =>
            s.get.reaction match {
              case cr : CrimsonReaction => cr onProtect DamageEvent(acc, None, player)
              case _ => acc
            }
          })
        }
      }
    }
  }



  val discolorPhase = "discolor phase"
  def discolor = { env : Env =>
    import env._
    getOwnerSelectedSlot().destroy()
	
	val houseId = (player.getHouses.foldLeft((0, 0, 0)) {
		case ((hidx, m, idx), h) ⇒
			if (h.mana > m) (idx, h.mana, idx + 1) else (hidx, m, idx + 1)
		})._1
	player.houses.incrMana(2, houseId)
		  
    player addTransition WaitPlayer(playerId, discolorPhase)
  }


  def getData(s : SlotState) : Int = {
    s.data match {
      case ColorData(x) => x
      case _ => 0
    }
  }
  def getData(s : SlotUpdate) : Int = getData(s.get)
  def getData(p : PlayerUpdate) : Int = {
    p.pstate.data match {
      case ColorData(x) => x
      case _ => 0
    }
  }
  def selectColour(playerId : PlayerId, state : GameState) = listSlotWhere(playerId, state)(_.houseId == Colors.houseId)
}

case class ColorData(x : Int)

case class Lower2XCostMod(x : Int) extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards map {  c ⇒
      if (c.card.cost <= 2 * x) {
        c.copy(cost = math.max(c.cost - 1, 0))
      } else c
    }
  }
}

case class HideCardMod(card : Card) extends DescMod {

  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex != card.houseIndex) cards
    else cards map { c => if (c.card == card) c.copy(enabled = false) else c }
  }
}