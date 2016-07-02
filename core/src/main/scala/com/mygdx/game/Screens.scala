package com.mygdx.game

import collection.JavaConverters._
import com.badlogic.gdx.scenes.scene2d.{Actor, Group}

import scala.util.control.NonFatal
import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx._
import com.mygdx.game.gui.{GameSettings, LobbyBoard}
import priv.sp.GameResources
import priv.util.GuiUtils._

class PresStartScreen() extends ScreenAdapter {

}

// common resources of the screens
class Screens(val game : Game) {
  val screenResources       = new ScreenResources
  val gameResources         = new GameResources
  var lastE                 = Option.empty[Throwable]
  val lobbyScreen           = new LobbyScreen(this)
  val gameScreen            = new GameScreen(this)

  import screenResources._

  def resize (width : Int, height : Int) {
    stage.getViewport.update(width, height, true)
  }

  @inline def render (delta : Float): Unit = {
    try {
      beforeProcess()
      stage act delta

      Gdx.gl glClear GL20.GL_COLOR_BUFFER_BIT
      stage.draw()
      engine update delta
      lastE = None
    } catch {
      case NonFatal(e) =>
        if (! lastE.contains(e) ) {
          lastE = Some(e)
          e.printStackTrace()
        }
    }
  }

  var isDebug = false
  def initInput(group : Group, scrollScreen : Boolean = false)(handleKey : Int => Boolean) = {
    Gdx.input.setInputProcessor(
      new InputMultiplexer(Gdx.input.getInputProcessor,
        gameScreen.screens.screenResources.stage,
        new InputAdapter(){
          override def keyDown(k : Int) = {
            if (k == Input.Keys.F5) {
              Gdx.app.log("input", "reload resources")
              gameScreen.screens.screenResources.reload()
              true
            } else if (k == Input.Keys.F6) {
              isDebug = !isDebug
              Gdx.app.log("input", "set debug " + isDebug)
              setDebug(group)
              true
            } else {
              handleKey(k)
            }
          }

          override def scrolled(amount : Int) = {
            if (scrollScreen) {
              scroll(-30 * amount)
            }
            true
          }

          private var y = 0
          private val h = 768
          private def scroll(delta : Int) = {
            val newy = y + delta
            val dy =
              if (newy<0) -y
              else if (newy > 2 *h) 2 * h -y
              else delta
            y = y + dy
            gameScreen.screens.screenResources.stage.getCamera.translate(0, dy, 0)
          }
        }))
  }

  def setDebug(group : Group) : Unit = {
    group setDebug isDebug
    group.getChildren.asScala foreach {
      case g : Group => setDebug(g)
      case a : Actor => a.setDebug(isDebug)
    }
  }

  def dispose(): Unit = {
    screenResources.dispose()
    gameResources.dispose()
  }
}

class LobbyScreen(screens : Screens) extends ScreenAdapter {
  import screens._
  val board = new LobbyBoard(screens)

  board.newGameButton addListener onClick {
    gameScreen.select()
    new LocalGameScreenContext(gameScreen)
  }
  board.settingsButton addListener onClick {
    screenResources.stage addActor new GameSettings(gameResources, screenResources)
  }

  def select() : Unit = {
    screenResources.clear()
    game setScreen this
    initInput(board.panel) { _ => false }
    screenResources.stage addActor board.panel
  }

  override def render (delta : Float): Unit = screens.render(delta)

  override def resize (width : Int, height : Int) = screens.resize(width, height)
  override def dispose(): Unit = screens.dispose()
}


class GameScreen(val screens : Screens) extends ScreenAdapter {
  import screens._

  override def render (delta : Float): Unit = screens.render(delta)

  def resetScreen[A](f : => A) : A = {
    screenResources.engine.getSystems.toArray.foreach(_.setProcessing(false))
    // FIXME should wait here
    screenResources.clear()
    val res = f
    screenResources.engine.getSystems.toArray.foreach(_.setProcessing(true))
    res
  }

  def returnToStart() = {
    resetScreen(())
    lobbyScreen.select()
  }

  def select() : Unit = {
    resetScreen(())
    game setScreen this
  }

  override def resize (width : Int, height : Int) = screens.resize(width, height)
  override def dispose(): Unit = screens.dispose()

}

