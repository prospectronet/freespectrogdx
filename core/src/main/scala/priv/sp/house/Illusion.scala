package priv.sp.house

import collection._
import priv.sp._

class Illusion {
  import CardSpec._
  import GameCardEffect._
  import priv.sp.update.{PlayerUpdate, HouseEventListener}  
  import priv.sp._
  import priv.util.FuncDecorators  

  val Illusion = House("illusion", List(
    Spell("illusion.Madness", I18n("illusion.Madness.description"), effects = effects(Direct -> Madness)),
    new Creature("illusion.PhantomWarrior", Attack(4), 4, I18n("illusion.PhantomWarrior.description"), reaction = new PhantomWarriorReaction),
	Spell("illusion.Hypnosis", I18n("illusion.Hypnosis.description"), effects = effects(Direct -> Hypnosis)),
    new Creature("illusion.WallofReflection", Attack(0), 20, I18n("illusion.WallofReflection.description"), reaction = new WallofReflectionReaction),
    new Creature("illusion.SpectralAssassin", Attack(6), 22, I18n("illusion.SpectralAssassin.description"), effects = effects(Direct -> damage(12, isAbility = true))),
    new Creature("illusion.SpectralMage", Attack(7), 34, I18n("illusion.SpectralMage.description"), effects = effects(Direct -> SpectralMage)),
    new Creature("illusion.Oracle", Attack(8), 41, I18n("illusion.Oracle.description"), effects = effects(OnTurn -> Oracle)),
    new Creature("illusion.Hypnotist", Attack(5), 39, I18n("illusion.Hypnotist.description"),  effects = effects(Direct -> damageCreatures(5, isAbility = true)), reaction = ManaGrowthReaction(1, 4))),
	eventListener = Some(new CustomListener(new IllusionEventListener))
	)

	Illusion initCards Houses.basicCostFunc

	
	def Madness = { env: Env ⇒
		import env._
		
		otherPlayer.slots.foreach { slot ⇒
			slot.inflict(Damage(slot.get.attack, env, isSpell = true))
		}
	}
	
	class PhantomWarriorReaction extends Reaction {
		override def inflict(damage: Damage): Unit = {
		  super.inflict(damage.copy(amount = 1))
		}
	}
	
	private def Hypnosis = { env: Env ⇒
		import env._

		(otherPlayer.value.slots.values.map(_.attack)(breakOut): Seq[Int]).sorted(math.Ordering.Int.reverse).take(2) foreach { attack ⇒
			env.otherPlayer inflict Damage(attack, env, isAbility = true)
		}
		
	}
	
	class WallofReflectionReaction extends Reaction {
	  override def onMyDamage(damage: Damage) {
		selected.player.otherPlayer inflict Damage(damage.amount, damage.context, isAbility = true)
	  }
	}	
	
	class SpectralMageReaction extends Reaction {
		final override def onSummon(summoned: SummonEvent) {
			import summoned._
			player.otherPlayer.slots.foreach { slot ⇒
				slot.inflict(Damage(slot.get.card.cost, Context(selected.playerId, Some(slot.get.card), slot.num), isAbility = true))	
				slot.focus()
			}
		}
	}
  
	
	def SpectralMage = { env: Env ⇒
		import env._
		
		player.otherPlayer.slots.foreach { slot ⇒
			slot.inflict(Damage(slot.get.card.cost, env, isAbility = true))	
			//slot.focus()
		}
	}
  
	def Oracle = { env: Env ⇒
		import env._
		val d = Damage(env.player.getHouses(4).mana, env, isAbility = true)		
		player.otherPlayer inflict d
		focus()
	}
  
	
	  
	


	class IllusionEventListener extends HouseEventListener with OwnerDeathEventListener {
	}  
}
