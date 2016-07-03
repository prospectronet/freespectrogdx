package priv.sp.house

import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update.{SlotUpdate, PlayerUpdate, HouseEventListener}
import priv.util.FuncDecorators

import scala.util.Random

object Limbo {
  import CardSpec._

  val heavenKeeper = new Creature("limbo.heaven", Attack(3), 7,
    I18n("limbo.heaven.description"),
    reaction = new HeavenKeeperReaction)

  val hellKeeper = new Creature("limbo.hell", Attack(3), 10,
    I18n("limbo.hell.description"),
    reaction = new HellKeeperReaction)

  val baron = new Creature("limbo.baron", Attack(6), 36,
    I18n("limbo.baron.description"))

  val wraith = new Creature("limbo.wraith", Attack(1) add new WraithAttack, 14,
    I18n("limbo.wraith.description"),
    runAttack = MultiTargetAttack)

  val Limbo: House = House("limbo", List(
    heavenKeeper,

    hellKeeper,

    wraith,

    new Creature("limbo.tower", Attack(0), 21,
      I18n("limbo.tower.description"),
      reaction = new TowerReaction),

    Spell("limbo.nether", I18n("limbo.nether.description"),
      effects = effects(Direct -> netherGrasp)),

    new Creature("limbo.adjudicator", Attack(3), 21,
      I18n("limbo.adjudicator.description"),
      reaction = new SoulReaction),

    Spell("limbo.redemption", I18n("limbo.redemption.description"),
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> redeem)),

    baron),

    description = I18n("limbo.description"),
    effects = List(OnEndTurn -> cleanLimbo),
    eventListener = Some(new CustomListener(new LimboEventListener)))

  Limbo initCards Houses.basicCostFunc

  class HeavenKeeperReaction extends Reaction with LimboReaction {
    def onLimbo(s : SlotUpdate): Unit = {
      val p = selected.player
      p heal 2
      p.slots healCreatures 2
      selected.focus()
    }
  }

  class HellKeeperReaction extends Reaction with LimboReaction {
    def onLimbo(s : SlotUpdate): Unit = {
      val p = selected.player.otherPlayer
      p heal 2
      p.slots inflictCreatures Damage(2, Context(selected.playerId, Some(hellKeeper), selected.num), isAbility = true)
      selected.focus()
    }
  }

  class TowerReaction extends Reaction with LimboReaction {
    def onLimbo(s : SlotUpdate): Unit = {
      s.value foreach { slotState =>
        val houseIndex = slotState.card.houseIndex
        selected.player.houses.incrMana(1, houseIndex)
        selected.player.otherPlayer.houses.incrMana(-1, houseIndex)
        selected.focus()
      }
    }
  }

  class SoulReaction extends Reaction with LimboReaction {
    val possibleCards = Set(heavenKeeper, hellKeeper)

    def onLimbo(s : SlotUpdate): Unit = {
      selected.player.pstate.desc.get.houses(4).cards.headOption.foreach { c =>
        c.card match {
          case creature : Creature if possibleCards contains creature =>
            val emptyAdjs = selected.adjacentSlots filter (_.value.isEmpty)

            (Random shuffle emptyAdjs).headOption foreach { s =>
              s add creature
              s setData NoLimbo
              selected.focus()
            }
          case _ =>
        }
      }
    }
  }

  def netherGrasp = { env : Env =>
    val amount = env.player.slots.foldl(0) { (i, s) =>
      s inflict Damage(100, env) // HACK
      i + 3
    }
    env.player.otherPlayer.slots inflictCreatures Damage(amount, env, isSpell = true)
  }

  def redeem = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    if (slot.get.data == LimboState) {
      val card = slot.get.card
      slot.destroy()
      slot add card
    }
  }

  def cleanLimbo = { env : Env =>
    env.player.slots foreach{ s =>
      if (s.get.data == LimboState) {
        s setData NoLimbo
        s inflict Damage(100, env) // !! HACK for phoenix (can't destroy)
      }
    }
    if (env.player.pstate.data == LimboState) {
      env.player setData NoLimbo
      env.player inflict Damage(0, env)
    }
  }

  trait LimboReaction {
    def onLimbo(s : SlotUpdate)
  }

  class LimboEventListener extends HouseEventListener {

    def setLimbo(s : SlotUpdate) = {
      s.write(s.value.map(_.copy(life = 0)))
      s setData LimboState
      player.slots foreach { slot =>
        if (slot.get.card == wraith) {
          slot.attack.setDirty()
        }
        slot.get.reaction match {
          case reaction : LimboReaction => reaction onLimbo s
          case _ =>
        }
      }
    }

    override def onDeath() = {
      player.pstate.data == NoLimbo || {
        if (hasBaron(player)) {
          player setData LimboState
          player write player.pstate.copy(life = 0)
          false
        } else {
          player.pstate.data != LimboState
        }
      }
    }

    def hasBaron( p : PlayerUpdate) = {
      p.slots.slots.exists(s => s.value.isDefined && s.get.card == baron)
    }

    override def init(p: PlayerUpdate) = {
      super.init(p)
      p.slots.slots foreach { slot =>
        slot.delayedDestroy = (FuncDecorators decorate slot.delayedDestroy) update { f =>
          { d: Damage =>
            if (slot.value.isDefined){
              val slotState = slot.get
              if ((slotState.card == baron || !(d.isSpell || d.isAbility))
                && slotState.data != NoLimbo) {
                setLimbo(slot)
                if (slotState.card == baron && slotState.data != LimboState) {
                  slot.attack add DoubleAttackSource
                }
              } else {
                f(d)
              }
            }
          }
        }
        slot.overridableDestroy = (FuncDecorators decorate slot.overridableDestroy) update { f =>
          { () =>
            if (slot.value.isDefined){
              val slotState = slot.get
              if (slotState.card == baron && slotState.data != LimboState) {
                setLimbo(slot)
                slot.attack add DoubleAttackSource
              } else {
                f()
              }
            }
          }
        }
      }
      p.slots.onDead = (FuncDecorators decorate p.slots.onDead) after { dead ⇒
        player.slots foreach { slot =>
          if (slot.get.card == wraith) {
            slot.attack.setDirty()
          }
        }
      }
    }


  }

  class WraithAttack extends AttackStateFunc {

    def apply(attack: Int, player: PlayerUpdate): Int = {
      (attack
      + player.slots.slots.count(s => s.value.isDefined && s.get.data == LimboState)
      + (if (player.pstate.data == LimboState) 1 else 0))
    }
  }

  case object DoubleAttackSource extends AttackSlotStateFunc {
    def apply(attack: Int, slot: SlotUpdate) = {
      attack * 2
    }
  }

  case object LimboState
  case object NoLimbo
}
