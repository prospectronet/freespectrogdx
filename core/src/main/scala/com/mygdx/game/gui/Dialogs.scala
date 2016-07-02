package com.mygdx.game.gui

import java.io._
import java.net.URL
import java.nio.channels.Channels
import java.nio.file.{Files, Paths}
import java.util.zip.ZipInputStream

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.scenes.scene2d.Actor
import com.badlogic.gdx.scenes.scene2d.ui.{List => _, _}
import com.badlogic.gdx.scenes.scene2d.utils.ChangeListener
import com.badlogic.gdx.scenes.scene2d.utils.ChangeListener.ChangeEvent
import com.badlogic.gdx.utils.Scaling
import com.mygdx.game.ScreenResources
import priv.sp._
import priv.util.GuiUtils._

object Dialogs {
  def center(dialog : Dialog) : Unit = {

    dialog.setPosition(
      (Gdx.graphics.getWidth - dialog.getWidth) / 2,
      (Gdx.graphics.getHeight - dialog.getHeight) / 2)
  }


  def createBtn(name : String, table : Table)(f : => Unit) = {
    val btn = new TextButton(name, table.getSkin)
    btn.addListener(onClick{
      f
    })
    btn
  }
}

class GameSettings(resources : GameResources, screenResources : ScreenResources) extends Dialog("game settings", screenResources.skin2) { self =>
  import Dialogs.createBtn
  
  class PlayerChoice(id: PlayerId) {
    private val table = new Table

    val specials = resources.sp.houses.special
    var n = 0
    val choiceCheckBoxes : Map[String, CheckBox] = specials.map(_.label)
      .map { choice =>
        val checkbox = new CheckBox(choice, screenResources.skin2)
        table add checkbox
        n += 1
        if (n % 2 == 0) table.row()
        (choice, checkbox)
      }.toMap
    table.row()

    val group = column(priv.util.GuiUtils.row(
      createBtn("all", self) { choiceCheckBoxes foreach (_._2.setChecked(true)) },
      createBtn("clear", self) { choiceCheckBoxes foreach (_._2.setChecked(false)) },
      createBtn("sinist", self) { select(resources.sp.houses.sinist) },
      createBtn("others", self) { select(resources.sp.houses.others) },
      createBtn("bs", self) { select(resources.sp.houses.bs) }),
      table)

    resources.playerChoices(id) foreach (h => choiceCheckBoxes(h.label).setChecked(true))
    updateResources()

    def updateResources() : Unit = {
      val selecteds = choiceCheckBoxes.collect { case (choice, checkbox) if checkbox.isChecked => choice }.toSet
      resources.playerChoices = resources.playerChoices.updated(id, specials.filter(x ⇒ selecteds.contains(x.label)))
    }

    def select(houses: List[House]) : Unit = {
      houses.foreach { h ⇒
        choiceCheckBoxes(h.label).setChecked(true)
      }
    }

  }

  screenResources.renderSystem setProcessing false  // HACK to avoid entities on dialog

  val choices = playerIds map { id ⇒ new PlayerChoice(id) }
  val pane = new SplitPane(choices(owner).group, choices(opponent).group, false, screenResources.skin)

  getContentTable.add(pane)

  button("ok", 'ok)
  button("cancel", 'cancel)
  pack()

  Dialogs.center(this)

  protected override def result (obj: AnyRef) : Unit = {
    obj match {
      case 'ok =>
        choices.foreach(_.updateResources())
      case _ =>
    }
    screenResources.renderSystem setProcessing true
  }

}

class InitDialog(resources : ScreenResources) extends Dialog("Download assets",resources.skin){
  val assets = resources.assetPath
  assets.mkdirs()

  var imagePack = "original"

  val selectBox = new SelectBox[String](resources.skin)
  selectBox.addListener(new ChangeListener() {
    def changed (event : ChangeEvent , actor:  Actor ) {
      imagePack = selectBox.getSelected()
    }
  })
  selectBox.setItems("original", "custom")
  selectBox.setSelected(imagePack)
  val log = new TextArea("", resources.skin)
  log.setPrefRows(3)
  log.setDisabled(true)

  getContentTable.add(column(selectBox, log))

  button("ok", 'ok)

  Dialogs.center(this)

  protected override def result (obj: AnyRef) : Unit = {
    obj match {
      case 'ok =>
        download(new URL(resources.config.getString("imagepack.backgrounds" )), "backgrounds.zip")
        download(new URL(resources.config.getString("imagepack." + imagePack)), "images.zip")
      case _ =>
    }
  }

  def addLog(message : String) = {
    log.setText(log.getText + "\n" + message)
  }

  def download(url : URL, name : String): Unit = {
    val target = assets.file().getCanonicalPath + File.pathSeparator + name
    if (! new File(target).exists()) {
      addLog("Downloading " + url + " to " + target)
      val channel = Channels.newChannel(url.openStream())
      println(target + " already exists")
      val out = new FileOutputStream(target)
      out.getChannel().transferFrom(channel, 0, Long.MaxValue)
    }
    unzip(target)
  }

  val BUFFER = 2048
  def unzip(file : String): Unit = {
    addLog("Unzipping " + file)
    val fis = new FileInputStream(file)
    val zis = new ZipInputStream(new BufferedInputStream(fis))
    var entry = zis.getNextEntry
    while(entry != null) {
      addLog("Extracting: " +entry)
      var data = new Array[Byte](BUFFER)
      val target = new File(assets.file.getCanonicalPath + File.separator + entry.getName())
      if (entry.getName().endsWith("/")) {
        target.mkdirs()
      } else {
        if (! target.getParentFile.exists()) {
          target.getParentFile.mkdirs()
        }
        val fos = new FileOutputStream(target)
        val dest = new BufferedOutputStream(fos, BUFFER)
        var count = zis.read(data, 0, BUFFER)
        while (count != -1) {
          dest.write(data, 0, count)
          count = zis.read(data, 0, BUFFER)
        }
        dest.flush()
        dest.close()
      }
      entry = zis.getNextEntry
    }
    zis.close()
  }

}

class HouseDescription (house : House, gameState : => GameState, playerId : PlayerId, resources : ScreenResources)
  extends Dialog(house.label, resources.skin) {

  val table = new Table()
  val pane = new ScrollPane(table)

  if (house.description.nonEmpty){
    val label = new Label(house.description, resources.skin2)
    label.setWrap(true)
    table.add(label).colspan(4).width(750)
    table.row()
  }

  var n = 0
  house.allCards foreach { card =>
    val path = CardActors.getPath(card, house)
    try {
      val image = new Image(resources.atlas createSprite path)
      val descriptionPanel = new DescriptionPanel(resources, descWidth = 300)

      descriptionPanel update Some(Description.cardToDesc(gameState, playerId, card))
      table.add(image).height(image.getHeight).center()
      table.add(descriptionPanel.panel).growY().top().left().pad(10)
      n += 1
      if (n % 2 == 0) {
        table.row()
      }
    } catch {
      case e : Exception => throw new Exception("Failed loading " + path, e)
    }
  }

  getContentTable add pane
  button("ok", 'ok)
  setResizable(true)
  pack()
  setWidth(math.min(getWidth, 800))
  setHeight(math.min(getHeight, 750))
  Dialogs.center(this)
  resources.renderSystem setProcessing false

  protected override def result (obj: AnyRef) : Unit = {
    resources.renderSystem setProcessing true
  }
}
