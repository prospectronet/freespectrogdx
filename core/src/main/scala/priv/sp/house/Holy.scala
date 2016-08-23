package priv.sp.house

import priv.sp._

class Holy {
  import CardSpec._
  import GameCardEffect._
  import priv.sp.update._
  import priv.sp.update.{PlayerUpdate, HouseEventListener}  
  import priv.sp._
  import priv.util.FuncDecorators  

  val HolyGuard = new Creature("holy.HolyGuard", Attack(4), 23, I18n("holy.HolyGuard.description"), reaction = new HolyGuardReaction)
  
  val Holy = House("holy", List(
    new Creature("holy.Paladin", Attack(4), 9, I18n("holy.Paladin.description"), effects = effects(Direct -> healCreatures(4))),
    new Creature("holy.Monk", Attack(4), 13, I18n("holy.Monk.description"), reaction = new MonkReaction),
    HolyGuard,
    Spell("holy.DivineJustice", I18n("holy.DivineJustice.description"), inputSpec = Some(SelectOwnerCreature), effects = effects(Direct -> DivineJustice)),
    Spell("holy.DivineIntervention", I18n("holy.DivineIntervention.description"), effects = effects(Direct -> addMana(2, 0, 1, 2, 3), Direct -> heal(10))),
    Spell("holy.WrathofGod", I18n("holy.WrathofGod.description"), effects = effects(Direct -> WrathofGod)),
    new Creature("holy.Angel", Attack(8), 42, I18n("holy.Angel.description"), effects = effects(Direct -> addMana(3, 4))),
    new Creature("holy.Archangel", Attack(8), 48, I18n("holy.Archangel.description"), effects = effects(Direct -> Archangel))),
	eventListener = Some(new CustomListener(new HolyEventListener))
	)

	Holy initCards Houses.basicCostFunc
  
	class MonkReaction extends Reaction {
		final override def onMyDeath(dead: Dead) {
		  selected.player.houses.incrMana(2, 4)
		  selected.focus()
	  }
	}
	


	class HolyGuardReaction extends Reaction {
		final override def onProtect(d: DamageEvent) = {
		  import d._
		  target match {
			case Some(num) if math.abs(num - selected.num) == 1  ⇒
			  val amount = d.damage.amount
			  d.damage.copy(amount = math.max(0, amount - 2))
			case _ ⇒ d.damage
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
	

	def WrathofGod = { env: Env ⇒
		import env._
		otherPlayer.slots foreach { slot ⇒
		  val d = Damage(12, env, isSpell = true)
		  slot.inflict(d)
		  if (slot.value.isDefined) {
			player.houses.incrMana(1, 4)
		  }
		}
		
	}
	
	def Archangel = { env: Env ⇒
		import env._
		player.slots foreach { slot ⇒
            slot heal 100
        }		
	}
	
	
	  
	


	class HolyEventListener extends HouseEventListener with OwnerDeathEventListener {
	    
		def protect(slot: SlotUpdate, damage: Damage) = {
		  player.slots.foldl(damage) { (acc, s) ⇒
			val c = s.get.card
			if (c == HolyGuard) {
			  s.get.reaction onProtect DamageEvent(acc, Some(slot.num), player)
			} else acc
		  }
		}
		
		override def init(p: PlayerUpdate) {
		  super.init(p)
		  p.slots.slots foreach { slot ⇒
			slot.protect modifyResult (d ⇒ protect(slot, d))
		  }
		}
	}  
}
