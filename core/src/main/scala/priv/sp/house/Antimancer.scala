package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect.Env
import priv.sp.update.{PlayerUpdate, HouseEventListener}
import priv.sp._
import priv.util.FuncDecorators

object Antimancer {

  val angryMob = new Creature("antimancer.0.name", Attack(3), 10, 
  I18n("antimancer.0.description"),
  effects = effects(OnTurn -> angryMobEffect))

  val retaliator = new Creature("antimancer.4.name", Attack(5), 8,
    I18n("antimancer.4.description"),
    reaction = new RetaliatorReaction)

  val bombardier = new Creature("antimancer.8.name", Attack(5), 45,
    I18n("antimancer.8.description"),
    reaction = new BombardierReaction,
	runAttack = new BombardierAttack)

  val Antimancer: House = House("antimancer", List(

    new Creature("antimancer.2.name", Attack(2), 15,
      I18n("antimancer.2.description"),
      reaction = new MirrorPriestReaction),

	retaliator,
		  
    Spell("antimancer.3.name", 
	  I18n("antimancer.3.description"),
      effects = effects(Direct -> resist)),

    new Creature("antimancer.5.name", Attack(6), 17,
      I18n("antimancer.5.description"),
      reaction = new MartyrReaction),

    new Creature("antimancer.6.name", Attack(4), 40,
      I18n("antimancer.6.description"),
      reaction = new HarvesterReaction),

    new Creature("antimancer.7.name", Attack(7), 31,
      I18n("antimancer.7.description"),
      effects = effects(OnTurn -> voodoo)),

    bombardier,

    Spell("antimancer.9.name", 
	  I18n("antimancer.9.description"),
      inputSpec = Some(SelectTarget(cost97OrInf)),
      effects = effects(Direct -> bribe))
  ),
  eventListener = Some(new CustomListener(new AntimancerListener)))
  
  private def angryMobEffect = { env: Env ⇒
    val damage = Damage(1, env, isAbility = true)
    val targets = env.player.slots(env.selected).adjacentSlots filter (_.value.isDefined)
    if (targets.nonEmpty) {
      env.focus()
      targets foreach (_.inflict(damage))
    }
  }

  Antimancer initCards { i: Int ⇒ i + 2 }
  Antimancer addAdditionalCards angryMob

  def cost7OrInf(p: PlayerId, state: GameState): List[Int] = {
    state.players(p).slots.foldLeft(List.empty[Int]) {
      case (acc, (i, s)) ⇒
        if (s.card.cost < 8) i :: acc else acc
    }
  }
  
  def cost97OrInf(p: PlayerId, state: GameState): List[Int] = {
    state.players(p).slots.foldLeft(List.empty[Int]) {
      case (acc, (i, s)) ⇒
		if (s.card.houseIndex == 4) {
			if (s.card.cost < 7) i :: acc else acc
		} else {
			if (s.card.cost < 10) i :: acc else acc
        }
    }
  }  

  def resist = { env : Env =>
    def spawnCreature(num: Int) : Unit = {
      val slot = env.player.slots(num)
      if (slot.value.isEmpty && slot.oppositeSlot.value.isEmpty) {
        slot add angryMob
      }
    }
    env.player.value.slotList foreach spawnCreature
  }

  def bribe = { env : Env =>
    env.updater.randLogs.unorderedShuffle(env.player.slots.getOpenSlots).headOption foreach { s =>
      val oppSlot = env.otherPlayer.slots(env.selected)
      val card = oppSlot.get.card
      oppSlot.destroy()
      s add card
	  s toggle runFlag
    }
  }

  def voodoo = { env : Env =>
    val d = Damage(1, env, isAbility = true)
    val filleds = env.otherPlayer.slots.filleds
    filleds foreach { slot => slot drain d }
    env.player heal filleds.size
  }
  
  class RetaliatorReaction extends Reaction {
    final override def onMyDamage(damage: Damage) {
      selected.player.otherPlayer.slots inflictCreatures Damage(damage.amount, Context(selected.playerId, Some(retaliator), selected.num), isAbility = true)
      selected.focus()
    }
  }

  class MirrorPriestReaction extends Reaction {

    final def onSubmit(c : Command) = {
	  if(c.card.houseIndex < 4 && !c.card.isSpell){
		selected.player.houses.incrMana(1, c.card.houseIndex)
		selected.focus()
	  }
    }
  }

  class MartyrReaction extends Reaction {

    final override def onDeath(dead: Dead) : Unit = {
      if (dead.isSpell || dead.isDestroy) {
		import dead._
		if(selected.playerId == player.id){
			selected.player.slots(dead.num) add dead.card
			selected.focus()
		}
      }
    }
  }

  class HarvesterReaction extends Reaction {

    final override def onDeath(dead: Dead) : Unit = {
	  selected.otherPlayer.houses.incrMana(-1, dead.card.houseIndex)
	  selected.focus()
    }
  }

  class BombardierReaction extends Reaction {
    final def onPlayerDamage(damage: Damage) = {
      if (damage.context.selected == selected.num && damage.context.playerId == selected.playerId) {
        selected.otherPlayer.slots inflictCreatures damage
        selected.focus()
      }

    }
    final override def onMyDeath(dead: Dead) {
      selected.oppositeSlot inflict Damage(10, Context(selected.playerId, Some(bombardier), selected.num), isAbility = true)
    }
  }
  
  class BombardierAttack extends RunAttack {
	  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
		val num = target.head
		val otherPlayer = player.otherPlayer
		val slot = otherPlayer.slots(num)
		
		slot.value match {
		  case None ⇒
			otherPlayer inflict d
		  case Some(slotState) ⇒
			slot inflict d
		}
		
		nearestEmptySlot(num, otherPlayer) foreach { n ⇒
			otherPlayer.slots.move(num, n)
		}
	  }
	}


  class AntimancerListener extends HouseEventListener with AnyDeathEventListener {

    final def onPlayerDamage(damage: Damage) = {
      player.slots foreach { s =>
        s.get.reaction match {
          case r: BombardierReaction ⇒ r onPlayerDamage damage
          case _ ⇒
        }
      }
    }

    final override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.submitCommand = (FuncDecorators decorate p.otherPlayer.submitCommand) after { c ⇒
        player.slots foreach { s ⇒
          s.get.reaction match {
            case r: MirrorPriestReaction ⇒ r onSubmit c
            case _ ⇒
          }
        }
      }
      p.otherPlayer.onPlayerDamage = (FuncDecorators decorate p.otherPlayer.onPlayerDamage) after { d: Damage =>
        onPlayerDamage(d)
      }
    }
  }

}
