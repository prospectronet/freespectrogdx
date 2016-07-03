package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.ui.Label
import com.mygdx.game._
import com.mygdx.game.net.NetPanel
import priv.util.GuiUtils._
import priv.sp.I18n

class LobbyBoard(screens : Screens) {

  val title          = new Label(I18n("lobby.message"),screens.screenResources.skin2)
  val buttonPanel    = new ButtonPanel(screens.screenResources.skin2)
  val newGameButton  = buttonPanel.getButton(I18n("button.singlegame"))
  val settingsButton = buttonPanel.getButton(I18n("button.settings"))
  val netPanel       = new NetPanel(screens, buttonPanel)
  val panel          = column(
    title, buttonPanel.panel, netPanel.panel)

  panel.fill()
  panel.setFillParent(true)
  panel.pack()
  panel.setY(panel.getY - 5)
}

