package priv.sp.house

import collection._
import priv.sp._

trait EarthHouse {
  import CardSpec._
  import GameCardEffect._

  val Earth: House = House("earth", List(
    new Creature("earth.elven", Attack(2), 12, I18n("earth.elven.description"), effects = effects(OnTurn -> focus(heal(3)))),
    Spell("earth.care", I18n("earth.care.description"),
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> heal(8), Direct -> healCreature(8))),
    new Creature("earth.sprite", Attack(1), 22, I18n("earth.sprite.description"), runAttack = MultiTargetAttack),
    Spell("earth.therapy",
      (state : GameState, playerId : PlayerId) => I18n.bundle.format("earth.therapy.description", (2 * state.players(playerId).houses(3).mana).toString),
      effects = effects(Direct -> { env: Env ⇒ env.player.heal(2 * env.getMana(3)) })),
    new Creature("earth.hermit", Attack(1), 13, I18n("earth.hermit.description"), effects = effects(OnTurn -> addMana(2, 3))), //, reaction = ManaGrowthReaction(2, 3)),
    Spell("earth.fury", (state : GameState, playerId : PlayerId) =>
      I18n.bundle.format("earth.fury.description", getFuryAttack(state.players(playerId)).toString),
      effects = effects(Direct -> fury)),
    new Creature("earth.hugespider", Attack(4), 24, I18n("earth.hugespider.description"), effects = effects(Direct -> spider)),
    new Creature("earth.troll", Attack(6), 26, I18n("earth.troll.description"), effects = effects(OnTurn -> focus(healCreature(4)))),
    Spell("earth.shower", I18n("earth.shower.description"), effects = effects(Direct -> massDamage(25, isSpell = true))),
    new Creature("earth.elemental", AttackSources().add(ManaAttack(3)), 50, effects = effects(OnTurn -> addMana(1, 3))),
    new Creature("earth.healer", Attack(3), 35, I18n("earth.healer.description"),
      effects = effects(OnTurn -> focus(heal(3)), OnTurn -> healCreatures(3))),
    new Creature("earth.hydra", Attack(3), 40, I18n("earth.hydra.description"),
      runAttack = MultiTargetAttack,
      effects = effects(OnTurn -> healCreature(4)))), houseIndex = 3)

  private val forestSpider = new Creature("earth.forestspider", Attack(2), 11) {
    cost = 1
  }

  Earth.addAdditionalCards(forestSpider)

  private def fury = { env: Env ⇒
    import env._

    // hack attack at start of transaction!
    val attack = getFuryAttack(player.value)
    env.otherPlayer inflict Damage(attack, env, isSpell = true)
  }

  def getFuryAttack(p : PlayerState) = {
    (p.slots.values.map(_.attack)(breakOut): Seq[Int]).sorted(math.Ordering.Int.reverse).take(2).sum
  }

  private def spider = { env: Env ⇒
    def spawnForestSpiderAt(num: Int) = {
      if (env.player.value.isInSlotRange(num)) {
        val slot = env.player.slots(num)
        if (slot.value.isEmpty) {
          slot.add(forestSpider)
        }
      }
    }
    spawnForestSpiderAt(env.selected - 1)
    spawnForestSpiderAt(env.selected + 1)
  }
}
