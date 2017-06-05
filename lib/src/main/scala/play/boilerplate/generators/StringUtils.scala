package play.boilerplate.generators

import java.io.File.{separator, separatorChar}

trait StringUtils {

  def sanitizeFileName(fileName: String): String = {
    val sep = if (separatorChar == 92.toChar) "\\\\" else separator
    fileName.split(sep)
      .toList
      .last
      .replace(".yaml", "")
      .replace(".json", "")
  }

  def objectNameFromFileName(fileName: String, obj: String): String = {
    sanitizeFileName(fileName).capitalize + obj
  }

  def padTo(n: Int, s: String): String = s + " " * (n - s.length max 0)

  def cleanDuplicateSlash(s: String): String = s.replaceAll("//+", "/")

  def composeName(parts: String*): String = {
    parts.filterNot(_.isEmpty).mkString(".")
  }

  def classNameToPath(packageName: String, className: String, ext: String): String = {
    val path = (packageName.split('.') :+ className).filterNot(_.isEmpty).mkString(separator)
    Seq(path, ext.dropWhile(_ == '.')).filterNot(_.isEmpty).mkString(".")
  }

}
