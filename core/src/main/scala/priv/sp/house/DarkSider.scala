package priv.sp.house

import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update._
import priv.util.FuncDecorators

object Darksider {
  import CardSpec._

  val wolfSpider = new Creature("darksider.spider.name", Attack(3), 16,
    I18n("darksider.spider.description"), reaction = new WolfSpiderReaction)
  val warlock = new Creature("darksider.warlock.name", Attack(5), 30,
    I18n("darksider.warlock.description"), reaction = new WarlockReaction)
  val blackKnight = new Creature("darksider.knight.name", Attack(4), 18,
    I18n("darksider.knight.description"), reaction = new BlackKnightReaction)

  val Darksider: House = House("darksider", List(
    new Creature("darksider.fallen.name", Attack(4), 12,
      I18n("darksider.fallen.description"), runAttack = new ElemAttack),
    wolfSpider,
    blackKnight,
    new Creature("darksider.mystic.name", AttackSources() add new ManaAttack(4), 21,
      I18n("darksider.mystic.description"),
      reaction = new DarkMysticReaction),
    new Creature("darksider.faceless.name", Attack(7), 34,
      I18n("darksider.faceless.description"),
      effects = effects(Direct -> horror)),
    new Creature("darksider.lake.name", Attack(0), 51,
      I18n("darksider.lake.description"), reaction = new LakeReaction, data = LakeData()),
    warlock,
    new Creature("darksider.dragon.name", Attack(8), 39,
      I18n("darksider.dragon.description"),
      reaction = new DragonReaction,
      effects = effects(Direct -> dragon))), eventListener = Some(new CustomListener(new DarksiderEventListener)))

  Darksider initCards Houses.basicCostFunc

  class WolfSpiderReaction extends Reaction {

    final override def onSummon(summoned: SummonEvent) {
      import summoned._
      if (selected.playerId != player.id) {
        val damage = Damage(card.cost, Context(selected.playerId, Some(wolfSpider), selected.num), isAbility = true)
        selected.focus()
        player.slots(selected.num) inflict damage
      }
    }
  }

  class BlackKnightReaction extends Reaction {
    def cond(num: Int): Boolean = math.abs(selected.num - num) == 1
    def applyEffect(s: SlotUpdate) = {
      val slotState = s.get
      if (slotState.card != blackKnight) {
        val target = if (slotState.target == List(s.num)) {
          List(selected.num)
        } else {
          selected.num :: slotState.target
        }
        s setTarget target
      }
    }

    def undoEffect(s: SlotUpdate) = {
      val slotState = s.get
      if (slotState.card != blackKnight) {
        val target = slotState.target filterNot (_ == selected.num)
        if (target.isEmpty) s setTarget List(s.num)
        else s setTarget target
      }
    }

    final override def onAdd(slot: SlotUpdate) = {
      if (selected.num == slot.num) {
        selected.filledAdjacents foreach { s ⇒
          applyEffect(s)
        }
      } else if (cond(slot.num)) {
        applyEffect(slot)
      }
    }

    final override def onRemove(slot: SlotUpdate) {
      if (cond(slot.num)) {
        undoEffect(slot)
      }
    }

    final override def onMyRemove(dead: Option[Dead]) = {
      selected.filledAdjacents foreach undoEffect
    }
  }

  class DarkMysticReaction extends Reaction {
    final override def onDeath(dead: Dead) {
      selected.player.houses.incrMana(1, 4)
    }
  }

  def horror = { env: Env ⇒
    env.getOwnerSelectedSlot().oppositeSlot.value foreach { s ⇒
      env.otherPlayer addDescMod Destroyed(s.card)
    }
  }

  case class LakeData(destroyeds: List[Destroyed] = Nil)
  class LakeReaction extends ReactionWithData[LakeData] {
    def onCommand(c: Command) {
      val destroyed = Destroyed(c.card)
      selected.otherPlayer addDescMod destroyed
      updateData(d ⇒ d.copy(destroyeds = destroyed :: d.destroyeds))
    }
    override def cleanUp() {
      getData.destroyeds foreach { d ⇒
        selected.otherPlayer removeDescMod d
      }
    }
  }

  class WarlockReaction extends Reaction {
    final def interceptSubmit(command: Command, updater: GameStateUpdater) = {
      if (command.card.isSpell) {
        selected inflict Damage(3 * command.card.cost, Context(selected.playerId, Some(warlock), selected.num), isAbility = true)
        selected.otherPlayer.houses.incrMana(-command.cost, command.card.houseIndex)
        (true, None)
      } else (false, None)
    }
  }

  def dragon = { env: Env ⇒
    env.otherPlayer.slots foreach (_.stun())
  }

  class DragonReaction extends Reaction {
    override def heal(amount: Int) {}
    override def inflict(damage: Damage) {
      if (!damage.isEffect) { super.inflict(damage) }
    }
    override def destroy() {}
    override def stun() {}
  }

  class DarksiderEventListener extends OwnerDeathEventListener {
    override def interceptSubmit(commandOption: Option[Command]): (Boolean, Option[Command]) = {
      commandOption match {
        case Some(c) if (c.player != player.id) ⇒
          player.slots.foldl((false, Option.empty[Command])) { (acc, s) ⇒
            if (acc._1) acc else {
              s.get.reaction match {
                case w: WarlockReaction ⇒ w.interceptSubmit(c, player.updater)
                case _                  ⇒ acc
              }
            }
          }
        case _ ⇒ (false, None)
      }
    }
    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.submitCommand = (FuncDecorators decorate p.otherPlayer.submitCommand) after { c ⇒
        player.slots foreach { s ⇒
          s.get.reaction match {
            case r: LakeReaction ⇒ r.onCommand(c)
            case _               ⇒
          }
        }
      }
    }
  }
}
