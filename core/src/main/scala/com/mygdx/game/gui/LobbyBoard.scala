package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.ui.Label
import com.mygdx.game._
import com.mygdx.game.net.NetPanel
import priv.util.GuiUtils._

class LobbyBoard(screens : Screens) {

  val title          = new Label("FreeSpectro is inspired by Spectromancer© (spectromancer.com) and made by a couple of fans",screens.screenResources.skin2)
  val buttonPanel    = new ButtonPanel(screens.screenResources.skin2)
  val newGameButton  = buttonPanel.getButton("Single game")
  val settingsButton = buttonPanel.SettingsButton
  val netPanel       = new NetPanel(screens, buttonPanel)
  val panel          = column(
    title, buttonPanel.panel, netPanel.panel)

  panel.fill()
  panel.setFillParent(true)
  panel.pack()
  panel.setY(panel.getY - 5)
}

