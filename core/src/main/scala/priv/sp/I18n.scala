package priv.sp

import java.util.{Locale, MissingResourceException}

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.utils.I18NBundle

object I18n {
  val locale = Option(System.getenv("LC_CTYPE")) map lcToLocale getOrElse Locale.getDefault()
  val isRussian = locale.getLanguage == "ru"

  val usedLocale = if (isRussian) locale else Locale.ENGLISH

  val bundle = I18NBundle.createBundle(Gdx.files.internal("i18n"), usedLocale)
  val defaultBundle = if (isRussian) I18NBundle.createBundle(Gdx.files.internal("i18n"), Locale.ENGLISH) else bundle

  def apply(key : String) = {
    try {
      bundle.get(key)
    } catch {
      case e : MissingResourceException => key
    }
  }

  def default(key : String) = {
    try {
      defaultBundle.get(key)
    } catch {
      case e : MissingResourceException => key
    }
  }

  private def lcToLocale(x : String) : Locale = Locale.forLanguageTag(x.replaceAll("_" ,"-"))
}
