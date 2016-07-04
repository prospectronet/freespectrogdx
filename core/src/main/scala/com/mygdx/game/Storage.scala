package com.mygdx.game

import java.io.File
import java.net.URL
import java.nio.file.Paths
import java.util.Properties

import com.badlogic.gdx.Gdx
import com.typesafe.config.Config
import priv.util.Utils

object Storage {
  val USER_NAME = "user.name"
  val CARD_THEME = "card.theme"
  val CLASS_CHOICE = List("class.choice", "ai.class.choice")
}

import Storage._

class Storage {

  val assetPath     = Gdx.files external ".freespectro"
  val themesPath    = Gdx.files external ".freespectro/themes"
  val userPrefsPath = Gdx.files external ".freespectro/user.prefs"
  val userPrefs     = new Properties()
  val checksum      = Utils.getChecksum() // store this? to check if changed to redownload assets (or try to check assets?)

  var userName       = Option.empty[String]
  var cardTheme      = "original"
  var classesChoices = List(List.empty[String], List.empty[String])

  if (userPrefsPath.exists()) {
    userPrefs.load(userPrefsPath.reader())
    initVars()
  }

  def fetchAssets(config : Config) : Unit = {
    val themePath = themesPath.file().getCanonicalPath + File.separator + cardTheme
    if (! java.nio.file.Files.exists(Paths get themePath)) {
      downloadAndUnzip(new URL(config getString "imagepack.backgrounds"), "backgrounds.zip", assetPath.file().getCanonicalPath)
      downloadAndUnzip(new URL(config getString ("imagepack." + cardTheme)), cardTheme+".zip", themePath)
    }
  }

  def persist(m : Map[String, String]) : Unit = {
    m foreach { case (k, v) => userPrefs.setProperty(k, v) }
    userPrefs.store(userPrefsPath.writer(false), "user preferences")
    initVars()
  }

  private def initVars() : Unit = {
    Option(userPrefs getProperty USER_NAME) foreach (x => userName = Some(x))
    Option(userPrefs getProperty CARD_THEME) foreach (x => cardTheme = x)
    CLASS_CHOICE.zipWithIndex foreach { case (key, idx) =>
      Option(userPrefs getProperty key) foreach (x => classesChoices = classesChoices.updated(idx, x.split(",").toList))
    }
  }

  private def downloadAndUnzip(url : URL, name : String, targetFolder : String) : Unit = {
    val tempFolder = assetPath.file().getCanonicalPath + File.separator + "tmp"
    val targetDownload = tempFolder + File.separator + name
    val targetFile = new File(targetDownload)
    if (! targetFile.exists()) {
      targetFile.getParentFile.mkdirs()
      println("Downloading " + url + " to " + targetDownload)
      Utils.download(url, name, targetDownload)
    }
    Utils.unzip(targetDownload, targetFolder)
  }
}
