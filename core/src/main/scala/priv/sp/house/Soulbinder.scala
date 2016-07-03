package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect.Env
import priv.sp._
import priv.sp.update.PlayerUpdate
import priv.util.FuncDecorators


object Soulbinder {

  val initState = BoundSouls()

  val soulSheppard = new Creature("soulbinder.sheppard", Attack(4).add(new SheppardAttack), 39,
    I18n("soulbinder.sheppard.description"),
    inputSpec = Some(SelectOwner(selectBoundSoul)),
    effects   = effects(OnTurn -> sheppard))

  val Soulbinder = House("soulbinder", List(

    new Creature("soulbinder.tethered", Attack(4), 17,
      I18n("soulbinder.tethered.description"),
      inputSpec = Some(SelectOwner(selectBoundSoul)),
      reaction = new TetheredReaction),

    Spell("soulbinder.summon",
      I18n("soulbinder.summon.description"),
      inputSpec = Some(SelectOwner(openSlotNoBoundSoul)),
      effects   = effects(Direct -> summon)),

    new Creature("soulbinder.tree", Attack(5), 21,
      I18n("soulbinder.tree.description"),
      inputSpec = Some(SelectOwner(selectBoundSoul)),
      effects   = effects(OnTurn -> tree)),

    new Creature("soulbinder.seeker", Attack(6), 26,
      I18n("soulbinder.seeker.description"),
      inputSpec = Some(SelectOwner(selectBoundSoul)),
      effects   = effects(OnEndTurn -> seek)),

    Spell("soulbinder.release",
      I18n("soulbinder.release.description"),
      effects = effects(Direct -> release)),

    soulSheppard,

    new Creature("soulbinder.priestess", Attack(7), 37,
      I18n("soulbinder.priestess.description"),
      inputSpec = Some(SelectOwner(selectBoundSoul)),
      effects   = effects(Direct -> passage),
      reaction = new PriestessReaction),

    new Creature("soulbinder.dragon", Attack(6), 44,
      I18n("soulbinder.dragon.description"),
      inputSpec = Some(SelectOwner(selectBoundSoul)),
      effects   = effects(Direct -> { env: Env ⇒
        env.focus()
        val damage = Damage(6, env, isAbility = true)
        env.otherPlayer inflict damage
        env.otherPlayer.slots inflictCreatures damage
      }, OnTurn -> dragon))
  ),
    data = initState,
    description = I18n("soulbinder.description"),
    effects = List(OnStart -> { env: Env ⇒ addSoulRandom(env.player)}),
    eventListener = Some(new CustomListener(new SoulListener))
  )

  Soulbinder.initCards(Houses.basicCostFunc)

  def selectBoundSoul(p: PlayerId, state: GameState): List[Int] = {
    val playerState = state.players(p)
    val slots = playerState.slots
    getBoundSouls(playerState).asList filter { x =>
      ! (slots isDefinedAt x) && (playerState.slotList contains x)
    }
  }

  def getBoundSouls(p : PlayerState) = {
    p.data match {
      case b : BoundSouls => b
      case _ => initState
    }
  }

  def openSlotNoBoundSoul(p: PlayerId, state: GameState): List[Int] = {
    val playerState = state.players(p)
    val souls = getBoundSouls(playerState).souls
    playerState.slotList filter { x =>
      ! (playerState.slots isDefinedAt x) && ! (souls contains x)
    }
  }

  def addSoulRandom(player : PlayerUpdate) : Boolean = {
    val souls = getBoundSouls(player.value).souls
    val openSlots = player.slots.getOpenSlots filter { s => ! (souls contains s.num) }
    if (openSlots.size > 0) {
      val num = openSlots(player.updater.randLogs get openSlots.size).num
      addSoul(player, num)
      true
    } else false
  }

  def addSoul(p : PlayerUpdate, x : Int) = {
    p.updateData[BoundSouls](d => d.copy(d.souls + x))
  }

  def getTotalSouls(env : Env) = {
    getBoundSouls(env.player.value).souls.size +
      (env.otherPlayer.value.data match {
        case BoundSouls(souls) => souls.size
        case _ => 0
      })
  }

  def dragon = { env : Env =>
    import env._
    val amount = 3 * getBoundSouls(player.value).souls.size
    getOwnerSelectedSlot() heal amount
    player heal amount
  }

  def sheppard = { env : Env =>
    val n = getBoundSouls(env.player.value).souls.size
    env.focus()
    env.player heal (2 * n)
  }

  def passage = { env : Env =>
    val total = getTotalSouls(env)
    env.player heal (2 * total)
    env.otherPlayer inflict Damage(2 * total, env, isAbility = true)
  }

  def release : Effect = { env : Env =>
    import env._
    val x = 3 * getTotalSouls(env)

    player heal x
    player.slots healCreatures x
    player setData initState
    otherPlayer.value.data match {
      case _ : BoundSouls => otherPlayer setData initState
      case _ =>
    }
    addSoulRandom(player)
    addSoulRandom(player)
  }

  def summon = { env : Env =>
    addSoul(env.player, env.selected)
    env.player.houses.incrMana(1, 0, 1, 2, 3)
  }

  def tree = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    val souls = getBoundSouls(env.player.value).souls
    slot.openAdjacents
      .find { s => ! (souls contains s.num) }
      .foreach { s => addSoul(env.player, s.num) }
  }

  def seek = { env : Env =>
    import env._
    val openSlots = player.slots.getOpenSlots
    if (openSlots.size > 0) {
      val num = openSlots(updater.randLogs get openSlots.size).num
      player.slots.move(selected, num)
      addSoul(player, selected)
    }
  }

  class TetheredReaction extends Reaction {
    override def onMyDeath(dead: Dead): Unit = {
      addSoul(selected.player, selected.num)
    }
  }

  class PriestessReaction extends Reaction {
    override def onDeath(dead : Dead) = {
      if (addSoulRandom(selected.player)) {
        selected.focus()
      }
    }
  }

  class SoulListener extends OwnerDeathEventListener {

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.setData = (FuncDecorators decorate p.setData) after { data =>
        p.slots foreach { s =>
          if (s.value.exists(_.card == soulSheppard)) {
            s.attack.setDirty()
          }
        }
        val souls = data.asInstanceOf[BoundSouls].souls
        if (souls.isEmpty) {
          p.addDescMod(HideSpecialCreatureMod)
        } else if (p.value.desc.descMods contains HideSpecialCreatureMod) {
          p removeDescMod HideSpecialCreatureMod
        }
      }
      p.slots.slots foreach { s =>
        s.add = (FuncDecorators decorate s.add) after { _ =>
          val souls = p.value.data.asInstanceOf[BoundSouls].souls
          if (souls contains s.num) {
            p.updateData[BoundSouls](d => d.copy(d.souls - s.num))
          }
        }
      }
    }
  }


  class SheppardAttack extends AttackStateFunc {
    def apply(attack: Int, player: PlayerUpdate): Int = {
      attack + getBoundSouls(player.value).souls.size
    }
  }
}

case class BoundSouls(souls : Set[Int] = Set.empty){
  val asList = souls.toList
}

object HideSpecialCreatureMod extends DescMod {

  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex == 4) cards map { c => if (c.card.isSpell) c else c.copy(enabled = false) }
    else cards
  }
}
