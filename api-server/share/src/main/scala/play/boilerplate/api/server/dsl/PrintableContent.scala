package play.boilerplate.api.server.dsl

import play.api.mvc._

trait PrintableContent[C] {
  def contentAsString(content: C): String
}

object PrintableContent {

  def apply[C](f: C => String): PrintableContent[C] = new PrintableContent[C] {
    override def contentAsString(content: C): String = f(content)
  }

  implicit val printableUnit: PrintableContent[Unit] = apply(_ => "")
  implicit val printableAnyContent: PrintableContent[AnyContent] = apply {
    case AnyContentAsEmpty =>
      ""
    case AnyContentAsText(txt) =>
      txt
    case AnyContentAsFormUrlEncoded(data) =>
      (for {
        (name, values) <- data
        value <- values
      } yield name + "=" + value).mkString("\n")
    case AnyContentAsRaw(raw) =>
      raw.toString()
    case AnyContentAsXml(xml) =>
      xml.map(scala.xml.Utility.trimProper).mkString
    case AnyContentAsJson(json) =>
      json.toString()
    case AnyContentAsMultipartFormData(data) =>
      data.toString
  }

}