package play.boilerplate.api.client.dsl

import scala.concurrent.Future

//object Compat extends AbstractCompat

trait Credentials[+T]
case object NoCredentials extends Credentials[Nothing]

trait RequestHandler[T] {
  def beforeRequest(operationId: String, request: Compat.WSRequest, credentials: Credentials[T] = NoCredentials): Future[Compat.WSRequest]
  def onSuccess(operationId: String, response: Compat.WSResponse, credentials: Credentials[T] = NoCredentials): Unit
  def onError(operationId: String, cause: Throwable, credentials: Credentials[T] = NoCredentials): Unit
}

object RequestHandler {

  def default[T]: RequestHandler[T] = new RequestHandler[T] {
    override def beforeRequest(operationId: String, request: Compat.WSRequest, credentials: Credentials[T]): Future[Compat.WSRequest] = Future.successful(request)
    override def onSuccess(operationId: String, response: Compat.WSResponse, credentials: Credentials[T]): Unit = ()
    override def onError(operationId: String, cause: Throwable, credentials: Credentials[T]): Unit = ()
  }

}