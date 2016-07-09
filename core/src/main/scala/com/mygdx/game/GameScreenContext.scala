package com.mygdx.game

import com.badlogic.gdx.{Gdx, Input}
import com.mygdx.game.net.RemoteOpponent
import priv.sp.Local
import priv.util.Utils._
import priv.util.GuiUtils._
import priv.sp.I18n

import scala.concurrent.Future

trait GameScreenContext {
  def gameScreen : GameScreen

}

class RemoteGameScreenContext(val gameScreen: GameScreen, opponent : RemoteOpponent) extends GameScreenContext {
  import gameScreen.screens._
  import net._

  val gameInit = new GameInit(screenResources, gameResources, opponent)
  initInput(gameInit.board.panel, scrollScreen = true)(_ => false)

  gameInit.userMenu.getButton(I18n("button.quit")) addListener onClick {
    opponent.client send Message(Header(MessageType.ExitDuel))
    opponent.client send Message(Header(MessageType.ListPlayers))
    opponent.client.messageQueue put GameSeed.PoisonPill
    gameInit.releaseLocks()
    gameScreen.returnToStart()
  }
}

class LocalGameScreenContext(val gameScreen: GameScreen) extends GameScreenContext {
  import gameScreen.screens._
  var currentGame = createGame()

  def createGame(): GameInit = gameScreen.resetScreen {
    if (currentGame != null) {
      currentGame.releaseLocks()
    }
    val gameInit = new GameInit(screenResources, gameResources, new Local(gameResources))
    screenResources.disconnectIfNeeded()

    gameInit.userMenu.getButton(I18n("button.new")) addListener onClick {
      currentGame = createGame()
    }
    gameInit.userMenu.getButton(I18n("button.quit")) addListener onClick {
      currentGame.releaseLocks()
      gameScreen.returnToStart()
    }
    localInit(gameInit)
  }


  def localInit(gameInit : GameInit) = {
    new LocalGameInit(gameInit)
    gameInit
  }

  class LocalGameInit(gameInit : GameInit) {
    import gameInit._

    userMenu.getButton(I18n("button.restart")).addListener(onClick {
      println("restart")
      switchNewGame {
        session.server.reset()
      }
    })

    initInput(gameInit.board.panel, scrollScreen = true){ k : Int =>
      if (k == Input.Keys.F8) {
        Gdx.app.log("input", "give mana")
        Future {
          session.giveMeMana()
        } (scala.concurrent.ExecutionContext.Implicits.global)
        true
      } else if (k == Input.Keys.F7) {
        println(session.state)
        true
      } else false
    }

    def switchNewGame(f : => Unit) {
      screenResources.engine.getSystems.toArray.foreach(_.setProcessing(false))
      board.cardPanels foreach (_.setEnabled(false))
      session.gameLock.release()
      session.gameLock = new priv.util.RichLock
      listener.lock.release()
      listener.lock = new priv.util.RichLock
      screenResources.engine.removeAllEntities()
      f
      screenResources.engine.getSystems.toArray.foreach(_.setProcessing(true))
      gameResources.gameExecutor submit runnable {
        session persistState session.server.initState
        session.start()
      }
    }
  }

}

