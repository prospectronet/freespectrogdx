package priv.sp.house

import priv.sp._
import priv.sp.update._

class Elementalist {
  import CardSpec._
  import GameCardEffect._

  val stoneGolem = new Creature("elementalist.golem", AttackSources(Some(7), Vector(StoneGolemAttackSource)), 30, I18n("elementalist.golem.description"),
    reaction = new SGReaction)

  val Elementalist = House("elementalist", List(

    new Creature("elementalist.sylph", Attack(5), 15, I18n("elementalist.sylph.description"),
      reaction = new SylphReaction, effects = effects(Direct -> sylphEffect)),

    Spell("elementalist.freeze", I18n("elementalist.freeze.description"),
      effects = effects(Direct -> freeze)),

    new Creature("elementalist.salamander", Attack(6), 14, I18n("elementalist.salamander.description"), effects = effects(OnTurn -> salamand)),

    Spell("elementalist.avalanche", (state : GameState, playerId : PlayerId) =>
      I18n.bundle.format("elementalist.avalanche.description", (2 * state.players(playerId).houses(3).mana).toString),
      effects = effects(Direct -> aval)),

    Spell("elementalist.incineration", I18n("elementalist.incineration.description"),
      effects = effects(Direct -> incinerate)),

    new Creature("elementalist.phoenix", Attack(9), 20, I18n("elementalist.phoenix.description"),
      effects = effects(OnTurn -> archPhoenixHeal)),

    stoneGolem,

    Spell("elementalist.frost", (state : GameState, playerId : PlayerId) =>
      I18n.bundle.format("elementalist.frost.description",
        getFrostLightX(state.players(playerId).houses, state.players(other(playerId)).houses).toString),
      inputSpec = Some(SelectTargetSlot),
      effects = effects(Direct -> frostLight))),
    eventListener = Some(new CustomListener(new ElementalistEventListener)))

  val sylph = Elementalist.cards(0)
  Elementalist.initCards(Houses.basicCostFunc)

  val sPhase = "sylph phase"
  def sylphEffect = { env: Env ⇒
    import env._
    player addDescMod IncrSylphCostMod
    player addDescMod HideSylphMod
    player addTransition WaitPlayer(env.playerId, sPhase)
    player addEffectOnce (OnEndTurn -> UnMod(HideSylphMod))
  }

	case object HideSylphMod extends DescMod {
	  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
		if (house.houseIndex == 4 || house.houseIndex == 2) cards
		else cards.map(_.copy(enabled = false))
	  }
	}

  def freeze = { env: Env ⇒
	env.otherPlayer.houses.incrMana(-1, 1)
    env.otherPlayer addDescMod SkipTurn
    env.otherPlayer addEffectOnce (OnEndTurn -> new Unfreeze(true))
  }

  // horror!
  class Unfreeze(chain: Boolean) extends Function[Env, Unit] {
    def apply(env: Env) {
      import env._
      player removeDescMod SkipTurn
      player removeEffect (_.isInstanceOf[Unfreeze])
      player addDescMod HideSpecialMod
      player addEffectOnce (OnEndTurn -> UnMod(HideSpecialMod))
      if (chain) {
        otherPlayer addDescMod SkipTurn
        otherPlayer addEffectOnce (OnEndTurn -> new Unfreeze(false))
      }
    }
  }

  def salamand = { env: Env ⇒
    import env._
    if (player.getHouses(0).mana > otherPlayer.getHouses(0).mana) {
      focus()
      otherPlayer inflict Damage(4, env, isAbility = true)
    }
  }

  def aval = { env: Env ⇒
    import env._
    val x = player.getHouses(3).mana
    otherPlayer.slots inflictCreatures Damage(2 * x, env, isSpell = true)
    player heal (2 * x)
    player.houses.incrMana(-x, 3)
  }

  def incinerate = { env: Env ⇒
    import env._
    def destroy(s: SlotUpdate) {
      val card = s.get.card
      s.overridableDestroy()
      s.slots.player addDescMod Destroyed(card)
    }
    (player.slots reduce lowestLife) foreach destroy
    (otherPlayer.slots reduce highestLife) foreach destroy
  }

  def archPhoenixHeal = { env : Env =>
    import env._
    val fireMana = player.houses.houses(0).mana
    if (fireMana > 0) {
      getOwnerSelectedSlot() heal fireMana
      focus()
    }
  }

  def frostLight = { env: Env ⇒
    import env._
    val x = getFrostLightX(player.getHouses, otherPlayer.getHouses)
    otherPlayer inflict Damage(x, env, isSpell = true)
    otherPlayer blockSlot selected
  }

  def getFrostLightX(houses : PlayerState.HousesType, oppHouses : PlayerState.HousesType) = {
    val opp = oppHouses.reduceLeft((h1, h2) ⇒ if (h1.mana < h2.mana) h1 else h2).mana
    val own = houses.reduceLeft((h1, h2) ⇒ if (h1.mana > h2.mana) h1 else h2).mana
    math.max(0, own - opp)
  }

  class SylphReaction extends Reaction {
    final override def onMyDeath(dead: Dead) {
      dead.player.removeDescMod(IncrSylphCostMod)
    }
  }

  class SGReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      // FiXME: hack watch if unblocked at start of "transaction"! for mass damage and now for titan
      val player = selected.player
      if (!player.updater.value.players(other(player.id)).slots.isDefinedAt(selected.num)
        && !player.otherPlayer.slots(selected.num).value.isDefined) {
        if (d.isEffect) {
          d.copy(amount = 0)
        } else d
      } else d
    }
  }

  case object IncrSylphCostMod extends DescMod {
    def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
      if (house.houseIndex < 4) cards
      else cards.map { c ⇒
        if (c.card == sylph) {
          c.copy(cost = c.cost + 1)
        } else c
      }
    }
  }


  case object StoneGolemAttackSource extends AttackSlotStateFunc {
    def apply(attack: Int, slot: SlotUpdate) = {
      if (slot.otherPlayer.getSlots isDefinedAt slot.num) {
        attack + 4
      } else {
        attack
      }
    }
  }

  class ElementalistEventListener extends HouseEventListener  {
    def refreshStoneGolem() {
      if (player.getSlots.values exists (_.card == stoneGolem)) {
        player.slots.filleds.withFilter(_.get.card == stoneGolem) foreach { s ⇒
          s.attack.setDirty()
        }
      }
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.slots.update after { _ ⇒ refreshStoneGolem() }
    }
  }
}
