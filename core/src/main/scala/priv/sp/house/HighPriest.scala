package priv.sp.house

import priv.sp._
import priv.sp.update._
import GameCardEffect._
import CardSpec._
import priv.util.FuncDecorators

/**
 * Introduced bullshit :
 * bennu -> doing something before dying = crap
 * serpent of ... ->
 *    crap spawning just after death
 *    doing something on opponent's turn
 *
 * localized bs:
 * eyes of wajet -> store nb card played (bugged for abilities)
 * simoom -> store nb turn ^^
 */
object HighPriest {

  val initState = HPriestData()
  val apis = new Creature("highpriest.Apis", Attack(4), 25,
	I18n("highpriest.Apis.description"),
    effects = effects(OnTurn -> apisEffect))

  val sphynx = new Creature("highpriest.sphinx", Attack(7), 24,
	I18n("highpriest.sphinx.description"),
    reaction = new SphinxReaction)

  val puzzle = new Creature("highpriest.puzzle", Attack(0), 6,
    I18n("highpriest.puzzle.description"),
    reaction = new PuzzleReaction)

  val ouroboros = new Creature("highpriest.Ouroboros", Attack(6), 38,
    I18n("highpriest.Ouroboros.description"),
    effects = effects(OnTurn -> ouro))

  val serpent = new Creature("highpriest.serpent", Attack(2), 8,
    I18n("highpriest.serpent.description"))

  val sunStone = new Creature("highpriest.sunstone", Attack(0), 22,
    I18n("highpriest.sunstone.description"),
    mod = Some(new SpellMod(x ⇒ x + 2)),
    effects = effects(OnTurn -> incrRaAttack))

  val guardianMummy = new Creature("highpriest.guardianmummy", Attack(4), 20)

  val dragonOfRa = new Creature("highpriest.Wingeddragon", Attack(6), 45,
    I18n("highpriest.Wingeddragon.description"),
    effects = effects(Direct -> ra))

  val babi = new Creature("highpriest.Babi", Attack(5), 21,
    I18n("highpriest.Babi.description"),
    reaction = new BabiReaction)

  val amit = new Creature("highpriest.Ammit", Attack(9), 39,
    I18n("highpriest.Ammit.description"),
    reaction = new AmitReaction)

  val hpSet = List[Card](
    new Creature("highpriest.Ancientcrocodile", Attack(9), 15, 
	  I18n("highpriest.Ancientcrocodile.description"),
      runAttack = new CrocodileAttack),
    new Creature("highpriest.Serpopard", Attack(4), 18,
      I18n("highpriest.Serpopard.description"),
      reaction = new SerpoReaction),
    new Creature("highpriest.Anubite", Attack(5), 20,
      I18n("highpriest.Anubite.description"),
      runAttack = new AnubiteAttack),
    babi,
    Spell("highpriest.Curseofchaos", 
	  I18n("highpriest.Curseofchaos.description"),
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> curse)),
    Spell("highpriest.Simoom",     			
		I18n("highpriest.Simoom.description"),
      effects = effects(Direct -> simoom)),
    amit,
    new Creature("highpriest.Apep", Attack(5), 42,   
		I18n("highpriest.Apep.description"),
      effects = effects(OnTurn -> apep, OnEndTurn -> apepOpp),
      runAttack = MultiTargetAttack))

  val HighPriest = House("highpriest", List(
    new Creature("highpriest.Sacredscarab", Attack(3), 11,
      I18n("highpriest.Sacredscarab.description"),
      reaction = new ScarabReaction),
    new Creature("highpriest.sunpriest", Attack(1), 16, 
	  I18n("highpriest.sunpriest.description"),
      runAttack = new SunPriestAttack),
    apis,
    new Creature("highpriest.Bennu", Attack(5), 21, 
	  I18n("highpriest.Bennu.description"),
      reaction = new BennuReaction),
    Spell("highpriest.EyeofWajet",
       I18n("highpriest.EyeofWajet.description"),
      effects = effects(Direct -> wajet)),
    sphynx,
    ouroboros,
    dragonOfRa),
    effects = List(OnTurn -> hpTurn, OnStart -> init),
    eventListener = Some(new CustomListener(new HPriestEventListener)),
    data = initState,
    description = I18n("highpriest.description")
	)

  HighPriest initCards Houses.basicCostFunc
  HighPriest.initCards(Houses.basicCostFunc, hpSet)
  HighPriest.addAdditionalCards(hpSet : _*)
  HighPriest.addAdditionalCards(guardianMummy, serpent, sunStone, puzzle)

  def getData(p: PlayerState) = {
    p.data match {
      case h : HPriestData => h
      case _ => initState
    }
  }

  def init = { env: Env ⇒ choosePath(env.player) }

  def choosePath(player: PlayerUpdate) {
    val slots = player.getSlots
    val (fireearth, waterair) = slots.values.foldLeft((0, 0)) {
      case (acc @ (f, w), s) ⇒
        val h = s.card.houseIndex
        if (h == 0 || h == 3) {
          (f + s.card.cost, w)
        } else if (h < 4) {
          (f, w + s.card.cost)
        } else acc
    }
    val hasMod = player.value.desc.descMods contains PathSet
    if (fireearth >= waterair) {
      if (hasMod) player removeDescMod PathSet
    } else if (!hasMod) player addDescMod PathSet
  }

  def hpTurn = { env: Env ⇒
    import env._
    player.updateData[HPriestData](x ⇒ x.copy(numTurn = x.numTurn + 1))
  }

  case object PathSet extends DescMod {
    def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
      if (house.houseIndex == 4) {
        cards map { c ⇒
          CardDesc(hpSet(c.card.cost - 1)) // bs index is cost -1
        }
      } else cards
    }
  }

  def wajet = { env: Env ⇒
    import env._
    val nbRevealeds = getData(player.value).revealeds.size
    player heal nbRevealeds
    player.slots healCreatures nbRevealeds
    otherPlayer.slots inflictCreatures Damage(nbRevealeds, env, isSpell = true)
  }

  def curse = { env: Env ⇒
    import env._
    val slot = otherPlayer.slots(selected)
    val slots = slot :: slot.filledAdjacents
    val damage = Damage(slots.map(_.get.attack).sum, env, isSpell = true)
    slots foreach(_ inflict damage)
  }

  def apisEffect: Effect = { env: Env ⇒
    import env._
    player.houses.incrMana(1, 4)
    val nbApis = player.slots.foldl(0) { (acc, s) ⇒
      if (s.get.card == apis) (acc + 1) else acc
    }
    if (nbApis > 1) {
      player heal (3 * (nbApis - 1))
      getOwnerSelectedSlot().focus()
    }
  }

  def ouro: Effect = { env: Env ⇒
    import env._
    nearestEmptySlot(selected, player) foreach { pos ⇒
      player.slots(pos) add serpent
    }
    otherPlayer addEffect (OnEndTurn -> new SerpentDie(selected))
  }

  def ra: Effect = { env: Env ⇒
    import env._
    nearestEmptySlot(selected, player) foreach { pos ⇒
      player.slots(pos) add sunStone
    }
  }

  def simoom = { env: Env ⇒
    import env._
    val nTurn = getData(player.value).numTurn
    val bonus = SimAttackReduction(nTurn)
    var maxAttack = 0
    otherPlayer.slots foreach { slot ⇒
      val att = slot.get.attack
      if (att > 2) {
        if (att > maxAttack) {
          maxAttack = att
        }
        slot.attack add bonus
      }
    }
    player addEffect (OnEndTurn -> SimoomRefresh(bonus, math.ceil(maxAttack / 3f).toInt))
  }

  def incrRaAttack = { env: Env ⇒
    import env._
    (player.slots findCard dragonOfRa) foreach { slot ⇒
      slot.attack add AttackAdd(1)
    }
  }

  def apep = { env: Env ⇒
    env.player.houses.incrMana(-1, 0, 1, 2, 3)
  }

  def apepOpp = { env: Env ⇒
    env.otherPlayer.houses.incrMana(-1, 0, 1, 2, 3)
  }

  // BULLSHIT (leak) this is crap. things happening on opponent's turn should belongs to opponent
  class SerpentDie(ouronumslot: Int) extends Function[Env, Unit] {
    def apply(env: Env) {
      import env._
      otherPlayer.getSlots foreach {
        case (i, s) ⇒
          if (s.card == serpent) {
            val slots = otherPlayer.slots
            val slot = slots(i)
            val amount = s.life
            otherPlayer heal amount
            (otherPlayer.getSlots get ouronumslot) match {
              case Some(s) if card == ouroboros ⇒ slots(ouronumslot) heal amount
              case _                            ⇒ player removeEffect (_ == this) // a bit bs should be in ouro reaction on his death
            }
            slot.focus()
            slot.destroy()
          }
      }
    }
  }

  class SphinxReaction extends Reaction {
    final override def onMyDeath(dead: Dead) {
      dead.player.slots(dead.num) add puzzle
    }
  }
  class PuzzleReaction extends Reaction {
    final override def onMyDeath(dead: Dead) {
      import dead._
      damage match {
        case Some(d) if !d.isEffect && d.context.playerId != player.id ⇒
          val slot = player.slots(dead.num)
          val slotState = player.slots.buildSlotState(slot, sphynx)
          slot add slotState.copy(life = slotState.life / 2)
        case _ ⇒
          val otherPlayer = player.otherPlayer
          val houseId = (otherPlayer.getHouses.foldLeft((0, 0, 0)) {
            case ((hidx, m, idx), h) ⇒
              if (h.mana > m) (idx, h.mana, idx + 1) else (hidx, m, idx + 1)
          })._1
          otherPlayer.houses.incrMana(-3, houseId)
      }
    }
  }

  class AnubiteAttack extends RunAttack {
    def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
      if (SingleTargetAttack.attack(target, d, player)) {
        nearestEmptySlot(d.context.selected, player).foreach { pos ⇒
          player.updater.focus(d.context.selected, player.id)
          player.slots(pos) add guardianMummy
        }
      }
    }
  }

  class BabiReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) = {
      if (selected.num == slot.num) {
        val mana = selected.otherPlayer.getHouses.map(_.mana).sum
        selected setData new Integer(mana)
      }
    }

    def reactIncrMana(houses: PlayerState.HousesType, selected: SlotUpdate) {
      val old = selected.get.data.asInstanceOf[Integer]
      if (old != null) { // not working for stranger
        val mana = houses.map(_.mana).sum
        selected setData new Integer(mana)
        if (mana > old) {
          val oppSlot = selected.oppositeSlot
          if (oppSlot.value.isDefined) {
            oppSlot inflict Damage(mana - old, Context(selected.playerId, Some(babi), selected.num), isAbility = true)
            selected.focus()
          }
        }
      }
    }
  }

  class AmitReaction extends Reaction {
    final override def onDeath(dead: Dead) {
      val power = dead.player.getHouses(dead.card.houseIndex).mana
      dead.player inflict Damage(power, Context(selected.playerId, Some(amit), selected.num), isAbility = true)
      dead.player.houses.incrMana(1, 4)
      selected.focus()
    }
  }

  case class SimAttackReduction(numTurn: Int) extends AttackStateFunc {
    def apply(attack: Int, player: PlayerUpdate): Int = {
      val curr = getData(player.updater.value.players(other(player.id))).numTurn
      2 + math.min((curr - numTurn) * 3, attack - 2)
    }
  }

  case class SimoomRefresh(bonus: SimAttackReduction, turnCount: Int) extends Function[Env, Unit] {
    val maxTurn = bonus.numTurn + turnCount
    def apply(env: Env) {
      import env._
      if (getData(player.value).numTurn > maxTurn) {
        player.otherPlayer.slots foreach { s ⇒
          s.attack removeAny bonus
        }
        player removeEffect (_ == this)
      } else {
        player.otherPlayer.slots foreach { s ⇒
          s.attack.setDirty()
        }
      }
    }
  }

  class HPriestEventListener extends HouseEventListener with AnyDeathEventListener {
    def onOppSubmit(command: Command) {
      val data = getData(player.value)
      val cardIdent = (command.card.houseIndex, command.card.cardIndex)
      if (!data.revealeds.contains(cardIdent)) {
        player.updateData[HPriestData](_.copy(revealeds = data.revealeds + cardIdent))
      }
    }

    override def init(p: PlayerUpdate) {
      super.init(p)

      if (p.value.desc.get.houses(4).house == HighPriest) { // hack for warp
        p.slots.update after { _ ⇒
          choosePath(player)
        }
      }
      p.otherPlayer.houses.update.after { houses ⇒
        player.slots foreach { s ⇒ // crap
          s.get.reaction match {
            case br: BabiReaction ⇒ br.reactIncrMana(houses, s)
            case _                ⇒
          }
        }
      }
      p.otherPlayer.submitCommand = (FuncDecorators decorate p.otherPlayer.submitCommand) after(onOppSubmit _)
    }
  }
}

class ScarabReaction extends Reaction {
  override def selfProtect(d: Damage) = {
    if (d.isSpell) d else {
      val nbAdjs = selected.adjacentSlots.count(_.value.isDefined)
      if (nbAdjs != 0) {
        d.copy(amount = math.max(0, d.amount - 2 * nbAdjs))
      } else d
    }
  }
}

class SunPriestAttack extends RunAttack {
  isMultiTarget = true

  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    SingleTargetAttack.apply(target, d, player)
    val massD = d.copy(amount = player.getHouses.minBy(_.mana).mana)
    player.otherPlayer.slots inflictCreatures massD
  }
}

class CrocodileAttack extends RunAttack {

  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    SingleTargetAttack.apply(target, d, player)
    player.slots(d.context.selected) toggle CardSpec.pausedFlag
    player addEffect (OnTurn -> new CountDown(2, { env: Env ⇒
      env.player.slots(d.context.selected) toggleOff CardSpec.pausedFlag
    }))
  }
}

// hack for zen guard
case object BennuDead
class BennuReaction extends Reaction {
  final override def onMyRemove(dead: Option[Dead]) {
    // Bullshit ?
    if (selected.get.data != BennuDead && !dead.exists(_.damage.exists(_.context.playerId == selected.playerId))) {
      selected.attack add AttackFactor(3f)
      selected setData BennuDead
      selected.updater.updateListener.refresh()
      selected.player.runSlot(selected.num, selected().get)
    }
  }
}

object SerpoBonus extends AttackFactor(2f)
class SerpoReaction extends Reaction {
  final override def onSummon(summoned: SummonEvent) = {
    import summoned._
    if (selected.num != num && card.houseIndex == 4 && selected.playerId == player.id) {
      nearestSlotOpposed(selected.num, player, opposed = false).foreach { n ⇒
        val slots = player.slots
        slots.move(selected.num, n)
        val att = slots(n).attack
        if (!att.has[SerpoBonus.type]) {
          att add SerpoBonus
          player addEffect (OnEndTurn -> { env: Env ⇒
            val s = env.player.slots(n)
            if (s.value.isDefined) {
              s.attack removeFirst SerpoBonus
            }
          })
        }
      }
    }
  }
}

case class HPriestData(revealeds: Set[(Int, Int)] = Set.empty, numTurn: Int = 0)

