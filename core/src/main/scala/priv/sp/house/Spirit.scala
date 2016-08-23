package priv.sp.house

import priv.sp._

class Spirit {
  import CardSpec._
  import GameCardEffect._
  import priv.sp.update.{PlayerUpdate, HouseEventListener}  
  import priv.sp._
  import priv.util.FuncDecorators  

  val Spirit = House("spirit", List(
    new Creature("spirit.Crusader", Attack(4), 15, I18n("spirit.Crusader.description"), effects = effects(Direct -> healCreatures(2))),
    new Creature("spirit.HolyAvenger", Attack(4), 23, I18n("spirit.HolyAvenger.description"), reaction = new HolyAvengerReaction),
    new Creature("spirit.Templar", Attack(4), 26, I18n("spirit.Templar.description"), reaction = new TemplarReaction),
    Spell("spirit.DivineJustice", I18n("spirit.DivineJustice.description"), inputSpec = Some(SelectOwnerCreature), effects = effects(Direct -> DivineJustice)),
    Spell("spirit.DivineMeddling", I18n("spirit.DivineMeddling.description"), effects = effects(Direct -> addMana(2, 0, 1, 2, 3), Direct -> damage(10, isSpell = true))),
    Spell("spirit.RageofGod", I18n("spirit.RageofGod.description"), effects = effects(Direct -> RageofGod)),
    new Creature("spirit.Angel", Attack(8), 42, I18n("spirit.Angel.description"), effects = effects(Direct -> addMana(3, 4))),
    new Creature("spirit.AngelofWar", Attack(8), 37, I18n("spirit.AngelofWar.description"),  effects = effects(Direct -> damageCreatures(8, isAbility = true), Direct -> healCreatures(8)))),
	eventListener = Some(new CustomListener(new SpiritEventListener))
	)

	Spirit initCards Houses.basicCostFunc
	val Templar = Spirit.cards(2)
  
	class HolyAvengerReaction extends Reaction {
	  final override def onDeath(dead: Dead) : Unit = {
		if (math.abs(dead.num - selected.num) == 1 ) {
		  selected.attack add AttackAdd(2)
		  selected.focus()
		}
	  }
	}
	
	class TemplarReaction extends Reaction {
	  final override def onSummon(summoned: SummonEvent) : Unit = {
		import summoned._
	    if (selected.playerId == player.id) {
			if (math.abs(summoned.num - selected.num) == 1 ) {
			  val damage = Damage(4, Context(selected.playerId, Some(Templar), selected.num), isAbility = true)
			  selected.player.otherPlayer inflict damage
			  selected.focus()
			}
		}
	  }
	}	
	
	def DivineJustice = { env: Env ⇒
		import env._
		player.slots(selected).heal(12)
		player.slots.foreach{ s ⇒
		  if(s != player.slots(selected))
		    s inflict Damage(12, env, isSpell = true)
		}
		otherPlayer.slots inflictCreatures Damage(12, env, isSpell = true)
    }
	

	def RageofGod = { env: Env ⇒
		import env._
		otherPlayer.slots foreach { slot ⇒
		  val d = Damage(12, env, isSpell = true)
		  slot.inflict(d)
		  if (slot.value.isDefined) {
			player.otherPlayer inflict Damage(3, env, isSpell = true);
			//player.houses.incrMana(1, 4)
		  }
		}
		
	}
	  
	


	class SpiritEventListener extends HouseEventListener with OwnerDeathEventListener {
	}  
}
