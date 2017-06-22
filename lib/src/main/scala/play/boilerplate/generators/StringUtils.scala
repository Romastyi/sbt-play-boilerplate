package play.boilerplate.generators

import java.io.File.{separator, separatorChar}

trait StringUtils {

  def decapitalize(s: String): String = {
    if (s == null) null
    else if (s.length == 0) ""
    else {
      val chars = s.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }
  }

  def sanitizeFileName(fileName: String): String = {
    val sep = if (separatorChar == 92.toChar) "\\\\" else separator
    fileName.split(sep)
      .toList
      .last
      .replace(".yaml", "")
      .replace(".json", "")
  }

  def objectNameFromFileName(fileName: String, obj: String, skipNotValidChars: Boolean = true): String = {
    stringToValidIdentifier(sanitizeFileName(fileName), skipNotValidChars).capitalize + obj
  }

  def stringToValidIdentifier(str: String, skipNotValidChars: Boolean): String = {

    def addUnderscore(sb: StringBuilder): Unit = {
      if (!skipNotValidChars) sb.append('_')
    }

    val sb = new StringBuilder()
    var afterUnderscore = false

    if (!Character.isJavaIdentifierStart(str.charAt(0))) {
      afterUnderscore = true
      addUnderscore(sb)
    }
    str.toCharArray.foreach { c =>
      if (Character.isJavaIdentifierPart(c)) {
        if (afterUnderscore) sb.append(c.toUpper) else sb.append(c)
        afterUnderscore = false
      } else if (!afterUnderscore) {
        afterUnderscore = true
        addUnderscore(sb)
      }
    }

    sb.mkString

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
