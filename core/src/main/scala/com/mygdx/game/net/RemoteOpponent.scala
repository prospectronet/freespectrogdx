package com.mygdx.game.net

import priv.sp._
import priv.util._

import priv.util.Utils._
object GameSeed {

  def create(resources : GameResources, name : String, oppName : String, oppHouses : List[Int]) = {
    val seed = System.currentTimeMillis
    val shuffle = new CardShuffle(resources.sp.houses)
    val choices = List(resources.playerChoices.head, oppHouses map resources.sp.houses.getHouseById)
    val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get(resources resolveChoices choices, owner)
    val state = GameState(List(PlayerState.init(p1State, p1Desc), PlayerState.init(p2State, p2Desc)))

    GameSeed(seed, GameDesc(Vector(p1Desc, p2Desc)), state, name, oppName)
  }

  val PoisonPill = new Object
}

case class GameSeed(seed : Long, desc : GameDesc, gameState : GameState, name : String, oppName : String )


class RemoteOpponent(
  resources : GameResources,
  val client : NetClient,
  val name : String,
  val playerId : PlayerId,
  val startingPlayer : PlayerId,
  gameSeed : GameSeed) extends GameServer {

  val seed      = gameSeed.seed
  val initState = gameSeed.gameState
  val desc      = gameSeed.desc
  val log       = new Log(this)

  def submitCommand(commandOption: Option[Command]) : Unit = {
    client.proxyMessage(commandOption)
  }
  
  def waitNextCommand(c: TVar[Option[Option[Command]]], state: GameState) : Unit = {
    log.debug("wait next command " + Thread.currentThread().getName)
    val msg = client.messageQueue.take()
    log.debug("taken " + msg + ", " +  Thread.currentThread().getName)
    msg match {
      case commandOption : Option[Command] => c set Some(commandOption)
      case GameSeed.PoisonPill => c.richLock.release()
      case _ => println("Unknown msg " + msg)
    }
  }

}
