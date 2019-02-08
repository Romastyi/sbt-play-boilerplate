package play.boilerplate.api.client.dsl

import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.libs.json.JsValue
import play.api.libs.ws._

object Compat extends AbstractCompat with JsonBodyWritables with JsonBodyReadables {

  override type WSClient = StandaloneWSClient
  override type WSRequest = StandaloneWSRequest
  override type WSResponse = StandaloneWSResponse

  implicit class WSResponseOps(val response: WSResponse) extends AnyVal {
    def json: JsValue = response.body[JsValue]
  }

  // Backport from Play WS

  implicit val writeableOf_urlEncodedForm: BodyWritable[Map[String, Seq[String]]] = {
    import java.net.URLEncoder
    BodyWritable[Map[String, Seq[String]]](
      transform = { formData =>
        val bytes = ByteString.fromString(formData.flatMap(item => item._2.map(c => item._1 + "=" + URLEncoder.encode(c, "UTF-8"))).mkString("&"))
        InMemoryBody(bytes)
      },
      contentType = "application/x-www-form-urlencoded; charset=utf-8"
    )
  }

  // multipart/form-data

  sealed trait Part
  final case class FilePart(key: String, filename: String, contentType: Option[String], ref: java.io.File) extends Part
  final case class DataPart(key: String, value: String) extends Part
  final case class MultipartFormData(dataParts: Map[String, Seq[String]], files: Seq[FilePart])

  def dataPart(key: String, value: Option[String]): Option[Part] = {
    value.map(DataPart(key, _))
  }

  def filePart(key: String, contentType: String, source: Option[java.io.File]): Option[Part] = {
    source.map(
      file => FilePart(key, file.getName, Some(contentType), file)
    )
  }

  def multipartFormData(parts: List[Option[Part]]): MultipartFormData = {
    MultipartFormData(
      dataParts = parts.collect {
        case Some(data: DataPart) => data
      }.groupBy(_.key).mapValues(_.map(_.value)),
      files = parts.collect {
        case Some(file: FilePart) => file
      }
    )
  }

  implicit val writeableOf_MultipartFormData: BodyWritable[MultipartFormData] = {

    import java.nio.file.{Files => JFiles}

    val boundary: String = "--------" + scala.util.Random.alphanumeric.take(20).mkString("")

    def formatDataParts(data: Map[String, Seq[String]]): ByteString = {
      val dataParts = data.flatMap {
        case (name, values) =>
          values.map { value =>
            s"--$boundary\r\nContent-Disposition: form-data; name=$name\r\n\r\n$value\r\n"
          }
      }.mkString("")
      ByteString.fromString(dataParts)
    }

    def filePartHeader(file: FilePart): ByteString = {
      val name = s""""${file.key}""""
      val filename = s""""${file.filename}""""
      val contentType = file.contentType.map { ct =>
        s"Content-Type: $ct\r\n"
      }.getOrElse("")
      ByteString.fromString(s"--$boundary\r\nContent-Disposition: form-data; name=$name; filename=$filename\r\n$contentType\r\n")
    }

    BodyWritable[MultipartFormData](
      transform = { form =>
        val bytes = formatDataParts(form.dataParts) ++ form.files.flatMap { file =>
          val fileBytes = ByteString.fromArray(JFiles.readAllBytes(file.ref.toPath))
          filePartHeader(file) ++ fileBytes ++ ByteString.fromString("\r\n")
        } ++ ByteString(s"--$boundary--")
        SourceBody(Source.single(bytes))
      },
      contentType = s"multipart/form-data; boundary=$boundary"
    )

  }

}
