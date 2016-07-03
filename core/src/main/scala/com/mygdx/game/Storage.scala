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
  val CLASS_CHOICE = List("class.choice", "ai.class.choice")
}

import Storage._

class Storage {

  val assetPath     = Gdx.files external ".freespectro"
  val packPath      = Gdx.files external ".freespectro/pack"
  val userPrefsPath = Gdx.files external ".freespectro/user.prefs"
  val userPrefs     = new Properties()
  val checksum      = Utils.getChecksum() // store this? to check if changed to redownload assets (or try to check assets?)

  var userName         = Option.empty[String]
  var classesChoices   = List(List.empty[String], List.empty[String])

  if (userPrefsPath.exists()) {
    userPrefs.load(userPrefsPath.reader())
    initVars()
  }

  def fetchAssets(config : Config) : Unit = {
    val p = packPath.file().getCanonicalPath()
    if (! java.nio.file.Files.exists(Paths get p)) {
      val imagePack = config getString "imagepack.choice"
      download(new URL(config getString "imagepack.backgrounds"), "backgrounds.zip")
      download(new URL(config getString ("imagepack." + imagePack)), "images.zip")
    }
  }

  def persist(m : Map[String, String]) : Unit = {
    m foreach { case (k, v) => userPrefs.setProperty(k, v) }
    userPrefs.store(userPrefsPath.writer(false), "user preferences")
    initVars()
  }

  private def initVars() : Unit = {
    Option(userPrefs getProperty USER_NAME) foreach (x => userName = Some(x))
    CLASS_CHOICE.zipWithIndex foreach { case (key, idx) =>
      Option(userPrefs getProperty key) foreach (x => classesChoices = classesChoices.updated(idx, x.split(",").toList))
    }
  }

  private def download(url : URL, name : String) : Unit = {
    val targetDownload = assetPath.file().getCanonicalPath + File.separator + name
    if (! new File(targetDownload).exists()) {
      assetPath.file().mkdirs()
      println("Downloading " + url + " to " + targetDownload)
      Utils.download(url, name, targetDownload)
    }
    Utils.unzip(targetDownload, assetPath.file().getCanonicalPath)
  }
}
