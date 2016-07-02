package priv.sp.house

import priv.sp._
import priv.sp.update._

object DarkPriest {
  import CardSpec._
  import GameCardEffect._

  val restlessSoul = new Creature("darkpriest.RestlessSoul.name", Attack(3), 11, 
	I18n("darkpriest.RestlessSoul.description"), 
	reaction = new RestlessReaction)
  val shadowPriest = new Creature("darkpriest.ShadowOfPriest.name", Attack(3), 13, 
	I18n("darkpriest.ShadowOfPriest.description"), 
	effects = effects(OnTurn -> shadowHeal))
  val heretic = new Creature("darkpriest.Heretic.name", Attack(6), 20, "")
  val blackAngel = new Creature("darkpriest.BlackAngel.name", Attack(8), 25, 
	I18n("darkpriest.BlackAngel.description"), 
	runAttack = new BlackAngelAttack)

  val DarkPriest: House = House("darkpriest", List(
    new Creature("darkpriest.Ghost.name", Attack(5), 16, 
		I18n("darkpriest.Ghost.description"), 
		reaction = new GhostReaction, effects = effects(Direct -> ghostHeal)),
    new Creature("darkpriest.Occultist.name", Attack(4), 20,   			
		I18n("darkpriest.Occultist.description"), 
		effects = effects(Direct -> occult)),
    Spell("darkpriest.BlackMass.name", (state : GameState, playerId : PlayerId) =>
		I18n("darkpriest.BlackMass.description") 
		+ " [" + getBlackMassX(state.players(other(playerId)).slots)+ "]"
      ,
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> blackMass)),
    new Creature("darkpriest.EnergyVampire.name", Attack(3), 23, 
		I18n("darkpriest.EnergyVampire.description"), 
		effects = effects(OnTurn -> evampire)),
    new Creature("darkpriest.BlackMonk.name", Attack(4), 25, 
		I18n("darkpriest.BlackMonk.description"), 
		reaction = new BlackMonkReaction),
    new Creature("darkpriest.Betrayer.name", Attack(7), 38, 
		I18n("darkpriest.Betrayer.description"), 
		inputSpec = Some(SelectTargetCreature), effects = effects(OnTurn -> betray)),
    new Creature("darkpriest.DarkHydra.name", Attack(1), 32, 
		I18n("darkpriest.DarkHydra.description"), 
		runAttack = new DarkHydraAttack),
    new Creature("darkpriest.Missionary.name", Attack(3), 36, 
		I18n("darkpriest.Missionary.description"), 
		effects = effects(Direct -> missionar), reaction = new MissionaryReaction)),
    effects = List(OnStart -> initRestless),
	description =  I18n("darkpriest.description"))

  val ghost = DarkPriest.cards(0).asCreature
  DarkPriest initCards Houses.basicCostFunc
  DarkPriest.addAdditionalCards(restlessSoul, shadowPriest, heretic, blackAngel)

  def missionar = { env: Env ⇒
    import env._
    val weakest = player.slots.foldl(Option.empty[SlotUpdate]) { (acc, s) ⇒
      if (s.num == selected) {
        acc
      } else {
        acc match {
          case None ⇒ Some(s)
          case Some(slot) ⇒
            if (slot.get.attack > s.get.attack) {
              Some(s)
            } else acc
        }
      }
    }
    weakest.foreach { w ⇒
      val houseIndex = w.get.card.houseIndex
      w.inflict(Damage(w.get.life / 2, env, isAbility = true))
      val weakestOther = otherPlayer.slots.foldl(Option.empty[SlotUpdate]) { (acc, s) ⇒
        if (s.get.card.houseIndex != houseIndex) {
          acc
        } else {
          acc match {
            case None ⇒ Some(s)
            case Some(slot) ⇒
              if (slot.get.attack > s.get.attack) {
                Some(s)
              } else acc
          }
        }
      }
      weakestOther foreach { slot ⇒ slot inflict Damage(slot.get.life / 2, env, isAbility = true) }
    }
  }
  def betray = { env: Env ⇒
    import env._
    val slot = env.player.slots(selected)
    val d = Damage(4, env, isAbility = true)
    focus()
    player inflict d
    slot inflict d
    slot.adjacentSlots foreach (_.inflict(d))
  }
  def evampire = { env: Env ⇒
    import env._
    val adjacentHouses = player.slots(selected).adjacentSlots flatMap(_.value) map (_.card.houseIndex)
    player.houses.incrMana(1, adjacentHouses: _*)
  }
  def blackMass = { env: Env ⇒
    import env._
    val slots = otherPlayer.slots.filleds
    val x = getBlackMassX(otherPlayer.slots.value)
    otherPlayer.slots inflictCreatures Damage(x, env, isSpell = true)
    player.slots(selected).destroy()
  }
  def getBlackMassX(slots : PlayerState.SlotsType) = {
    4 * (slots.values.map { s ⇒ s.card.houseId }(collection.breakOut) : List[Int]).distinct.size
  }
  def occult: Effect = { env: Env ⇒
    import env._
    val slot = otherPlayer.slots(selected)
    if (slot.value.isEmpty) {
      slot add shadowPriest
    }
  }
  def shadowHeal = { env: Env ⇒
    import env._
    focus()
    otherPlayer heal 1
    otherPlayer.slots healCreatures 1
  }
  def ghostHeal = { env: Env ⇒
    import env._
    val nbSlots = player.getSlots.size
    player heal nbSlots
  }
  def initRestless = { env: Env ⇒
    env.otherPlayer addEffect (OnEndTurn -> new SpawnRestless)
    spawnRestless(env.otherPlayer)
  }

  class SpawnRestless extends Function[Env, Unit] {
    def apply(env: Env) {
      import env._
      if (!player.slots().exists { case (n, slot) ⇒ slot.card == restlessSoul }) {
        spawnRestless(player)
      }
    }
  }

  def spawnRestless(player: PlayerUpdate) {
    val openSlots = player.slots.getOpenSlots
    if (openSlots.nonEmpty) {
      val slot = openSlots(scala.util.Random.nextInt(openSlots.size))
      slot add restlessSoul
      slot.focus(blocking = false)
    }
  }

  class RestlessReaction extends Reaction {
    final override def onMyDeath(dead: Dead) {
      dead.otherPlayer.houses.incrMana(2, 4)
    }
  }

  class GhostReaction extends Reaction {
    final override def onMyDeath(dead: Dead) {
      if (dead.isEffect) {
        val openSlots = dead.otherPlayer.slots.getOpenSlots
        if (openSlots.nonEmpty) {
          val slot = openSlots(scala.util.Random.nextInt(openSlots.size))
          slot add ghost
          slot.focus(blocking = false)
        }
      }
    }
  }
  class MissionaryReaction extends Reaction {
    final override def onSummon(summoned: SummonEvent) {
      import summoned._
      if (selected.playerId == player.id) {
        if (card.houseIndex == 4) {
          selected.destroy()
          selected add blackAngel
        } else {
          val slot = player.slots(num)
          slot.destroy()
          slot add heretic
        }
      }
    }
  }

  class BlackAngelAttack extends RunAttack {

    def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
      val num = target.head
      val otherPlayer = player.otherPlayer
      val slot = otherPlayer.slots(num)
      if (slot.value.isEmpty) {
        otherPlayer inflict d
      } else {
        slot inflict d
        // FIXME maybe not good at all and should add source in damage?
        if (slot.value.isEmpty) {
          player.slots(num) heal blackAngel.life
        }
      }
    }
  }
}

class BlackMonkReaction extends Reaction {
  override def onMyDamage(damage: Damage) {
    selected.player heal damage.amount
  }
}

class DarkHydraAttack extends RunAttack {
  isMultiTarget = true
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    otherPlayer inflict d
    otherPlayer.slots inflictCreatures d
    player heal d.amount
    val slot = player.slots(num)
    if (slot.value.isDefined) {
      slot.attack add OneAttackBonus // TODO refactor
    }
  }
}
