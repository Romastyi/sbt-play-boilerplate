package play.boilerplate.api.client.dsl

import play.boilerplate.api.Tracer

import scala.concurrent.Future

//object Compat extends AbstractCompat

trait Credentials[+T]
case object NoCredentials extends Credentials[Nothing]

trait RequestHandler[T] {
  def beforeRequest(operationId: String, request: Compat.WSRequest, credentials: Credentials[T] = NoCredentials)(implicit tracer: Tracer): Future[Compat.WSRequest]
  def onSuccess(operationId: String, response: Compat.WSResponse, credentials: Credentials[T] = NoCredentials)(implicit tracer: Tracer): Unit
  def onError(operationId: String, cause: Throwable, credentials: Credentials[T] = NoCredentials)(implicit tracer: Tracer): Unit
}

object RequestHandler {

  def default[T]: RequestHandler[T] = new RequestHandler[T] {
    override def beforeRequest(operationId: String, request: Compat.WSRequest, credentials: Credentials[T])(implicit tracer: Tracer): Future[Compat.WSRequest] = Future.successful(request)
    override def onSuccess(operationId: String, response: Compat.WSResponse, credentials: Credentials[T])(implicit tracer: Tracer): Unit = ()
    override def onError(operationId: String, cause: Throwable, credentials: Credentials[T])(implicit tracer: Tracer): Unit = ()
  }

}