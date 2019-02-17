package play.boilerplate.api.client.dsl

import play.api.http.Writeable
import play.api.mvc.MultipartFormData

object Compat extends AbstractCompat {

  override type WSClient = play.api.libs.ws.WSClient
  override type WSRequest = play.api.libs.ws.WSRequest
  override type WSResponse = play.api.libs.ws.WSResponse

  implicit class WSRequestOps(val request: WSRequest) extends AnyVal {
    def addHttpHeaders(headers: (String, String)*): WSRequest = request.withHeaders(headers: _ *)
  }

  implicit class WSResponseOps(val response: WSResponse) extends AnyVal {
    def headers: Map[String, Seq[String]] = response.allHeaders
  }

  // Backport from Play 2.6.x

  // multipart/form-data

  sealed trait Part
  final case class FilePart(key: String, filename: String, contentType: Option[String], ref: java.io.File) extends Part
  final case class DataPart(key: String, value: String) extends Part

  def dataPart(key: String, value: Option[String]): Option[Part] = {
    value.map(DataPart(key, _))
  }

  def filePart(key: String, contentType: String, source: Option[java.io.File]): Option[Part] = {
    source.map(
      file => FilePart(key, file.getName, Some(contentType), file)
    )
  }

  def multipartFormData(parts: List[Option[Part]]): MultipartFormData[java.io.File] = {
    MultipartFormData(
      dataParts = parts.collect {
        case Some(data: DataPart) => data
      }.groupBy(_.key).mapValues(_.map(_.value)),
      files = parts.collect {
        case Some(FilePart(key, filename, contentType, ref)) =>
          MultipartFormData.FilePart(key, filename, contentType, ref)
      },
      badParts = Nil,
      missingFileParts = Nil
    )
  }

  implicit val writeableOf_MultipartFormData: Writeable[MultipartFormData[java.io.File]] = {

    import java.nio.file.{Files => JFiles}

    import play.api.libs.iteratee.Execution.Implicits.trampoline

    val boundary: String = "--------" + scala.util.Random.alphanumeric.take(20).mkString("")

    def getBytes(s: String): Array[Byte] = s.getBytes("UTF-8")

    def formatDataParts(data: Map[String, Seq[String]]): Array[Byte] = {
      val dataParts = data.flatMap {
        case (name, values) =>
          values.map { value =>
            s"--$boundary\r\nContent-Disposition: form-data; name=$name\r\n\r\n$value\r\n"
          }
      }.mkString("")
      getBytes(dataParts)
    }

    def filePartHeader(file: MultipartFormData.FilePart[java.io.File]): Array[Byte] = {
      val name = s""""${file.key}""""
      val filename = s""""${file.filename}""""
      val contentType = file.contentType.map { ct =>
        s"Content-Type: $ct\r\n"
      }.getOrElse("")
      getBytes(s"--$boundary\r\nContent-Disposition: form-data; name=$name; filename=$filename\r\n$contentType\r\n")
    }

    Writeable[MultipartFormData[java.io.File]](
      transform = { form: MultipartFormData[java.io.File] =>
        formatDataParts(form.dataParts) ++ form.files.flatMap { file =>
          val fileBytes = JFiles.readAllBytes(file.ref.toPath)
          filePartHeader(file) ++ fileBytes ++ getBytes("\r\n")
        } ++ getBytes(s"--$boundary--")
      },
      contentType = Some(s"multipart/form-data; boundary=$boundary")
    )

  }

}
