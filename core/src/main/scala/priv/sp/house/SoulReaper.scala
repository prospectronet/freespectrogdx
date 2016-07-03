package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect.Env
import priv.sp._
import priv.sp.update.{SlotUpdate, PlayerUpdate, HouseEventListener}
import priv.util.FuncDecorators

object SoulReaper {

  val initData = SoulReaperData()

  val damnation = Spell("reaper.damnation", I18n("reaper.damnation.description"),
    inputSpec = Some(SelectTargetCreature),
    effects   = effects(Direct -> damn))

  val onSlaught = Spell("reaper.onslaught", I18n("reaper.onslaught.description"),
    effects   = effects(Direct -> onSlaughtEffect))

  val death = Spell("reaper.sentence", I18n("reaper.sentence.description"),
    effects = effects(Direct -> deathSentence))

  val SoulReaper: House = House("reaper", List(
    damnation,

    Spell("reaper.passage", I18n("reaper.passage.description"),
      inputSpec = Some(SelectOwnerCreature),
      effects   = effects(Direct -> pass)),

    Spell("reaper.rage", I18n("reaper.rage.description"),
      inputSpec = Some(SelectOwnerCreature),
      effects   = effects(Direct -> eternalRage)),

    onSlaught,

    Spell("reaper.tribute", I18n("reaper.tribute.description"),
      inputSpec = Some(SelectOwnerCreature),
      effects   = effects(Direct -> tribute)),

    Spell("reaper.furious", I18n("reaper.furious.description"),
      effects = effects(Direct -> furious)),

    death,

    Spell("reaper.shackles", I18n("reaper.shackles.description"),
      inputSpec = Some(SelectTargetCreature),
      effects   = effects(Direct -> shackle))
  ),
  data = initData,
  eventListener = Some(new CustomListener(new SoulReaperListener)),
  description = I18n("reaper.description"))

  SoulReaper initCards Houses.basicCostFunc

  def shackle = { env : Env =>
    import env._
    val slot = getTargetSelectedSlot()
    slot.value foreach { s =>
      slot remove None
      Warp.bridle(s.copy(attack = 0, attackSources = AttackSources(Some(0))), slot)
      otherPlayer blockSlot selected
      player addEffect (OnTurn -> new ShackleEffect(selected))
    }
  }

  class ShackleEffect(num : Int) extends Function[Env, Unit] {
    def apply(env : Env) = {
      val slot = env.player.slots(num)
      val oppSlot = slot.oppositeSlot
      if (oppSlot.value.isDefined) {
        env.player.updateData[SoulReaperData](data => data.copy(x = data.x + 1))
      } else if (slot.value.isDefined) {
        slot inflict Damage(6, env, isAbility = true)
      }
    }
  }

  def deathSentence = { env : Env =>
    def kill(slot : SlotUpdate): Unit = {
      slot.overridableDestroy()
      env.player.updateData[SoulReaperData](data => data.copy(x = data.x + 1))  // 1 already count in listener
    }
    env.player.slots foreach kill
    env.otherPlayer.slots foreach kill
  }

  def furious = { env : Env =>
    val x = getX(env.player)
    env.player setData initData
    env.otherPlayer inflict Damage(x + 7, env, isSpell = true)
  }

  def tribute = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    val life = slot.get.life
    env.player heal math.min(life, 36)
    slot.destroy()
  }

  def onSlaughtEffect = { env : Env =>
    env.otherPlayer.slots inflictCreatures Damage(getX(env.player), env, isSpell = true)
    env.player setData initData
  }

  def eternalRage = { env : Env => env.getOwnerSelectedSlot() setData Eternal }

  def pass = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    val x = math.min(12, getX(env.player))
    slot heal x
    slot.filledAdjacents foreach (_ heal x)
  }

  def damn = { env : Env =>
    val slot = env.getTargetSelectedSlot()
    val x = getX(env.player)
    slot inflict Damage(math.min(8, 3 + x), env, isSpell = true)
  }

  class SoulReaperListener extends HouseEventListener {
    val someOnslaught = Some(onSlaught)
    val someDamn = Some(damnation)

    def reactDead(playerId : PlayerId, dead : Dead) : Unit = {
      val isAttackFromOwner = dead.damage.exists(_.context.playerId == playerId)
      if (!(isAttackFromOwner && dead.damage.exists(_.context.card == someOnslaught))) {
        player.updateData[SoulReaperData](data => data.copy(x = data.x + 1))
        if (isAttackFromOwner && dead.damage.exists(_.context.card == someDamn)) {
          player.updateData[SoulReaperData](data => data.copy(x = data.x + dead.card.cost))
        }
      }
    }

    def checkEternal(s : SlotUpdate) = {
      val isEternal = s.get.data == Eternal
      if (isEternal) {
        s.value foreach { slotState =>
          if (slotState.life > 0) {
            s write Some(slotState.copy(life = 0))
            player addEffect (OnEndTurn -> new CountDown(2, { env: Env ⇒
              env.player.slots.findSlot(slotState.id) foreach (_.destroy())
            }))
          }
        }
      }
      isEternal
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.slots.onDead = (FuncDecorators decorate p.slots.onDead) after (dead => reactDead(p.id, dead))
      p.otherPlayer.slots.onDead = (FuncDecorators decorate p.otherPlayer.slots.onDead) after (dead => reactDead(p.id, dead))
      p.slots.slots foreach { slot =>
        slot.delayedDestroy = (FuncDecorators decorate slot.delayedDestroy) update { f =>
          { d: Damage =>
            if (!checkEternal(slot)) {
              f(d)
            }
          }
        }
        slot.overridableDestroy = (FuncDecorators decorate slot.overridableDestroy) update { f =>
          { () =>
            if (!checkEternal(slot)) {
              f()
            }
          }
        }
      }
    }
  }

  @inline def getData(p : PlayerUpdate) = {
    p.pstate.data match {
      case s : SoulReaperData => s
      case _ => initData
    }
  }
  @inline def getX(p : PlayerUpdate) = getData(p).x

  case object Eternal
}

case class SoulReaperData(x : Int = 0)
