package play.boilerplate.api.client.dsl

import akka.NotUsed
import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import play.api.mvc.MultipartFormData

import scala.concurrent.Future

object Compat extends AbstractCompat {

  override type WSClient = play.api.libs.ws.WSClient
  override type WSRequest = play.api.libs.ws.WSRequest
  override type WSResponse = play.api.libs.ws.WSResponse

  implicit class WSRequestOps(val request: WSRequest) extends AnyVal {
    def addHttpHeaders(headers: (String, String)*): WSRequest = request.withHeaders(headers: _ *)
  }

  // multipart/form-data

  def dataPart(key: String, value: Option[String]): Option[MultipartFormData.Part[Source[ByteString, Any]]] = {
    value.map(MultipartFormData.DataPart(key, _))
  }

  def filePart(key: String, contentType: String, source: Option[java.io.File]): Option[MultipartFormData.FilePart[Source[ByteString, Future[IOResult]]]] = {
    source.map(
      file => MultipartFormData.FilePart(key, file.getName, Some(contentType), FileIO.fromPath(file.toPath))
    )
  }

  def multipartFormData(parts: List[Option[MultipartFormData.Part[Source[ByteString, Any]]]]): Source[MultipartFormData.Part[Source[ByteString, Any]], NotUsed] = {
    Source(parts.flatten)
  }

}
