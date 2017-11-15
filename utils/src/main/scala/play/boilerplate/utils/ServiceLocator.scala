package play.boilerplate.utils

import java.net.URI

import com.typesafe.config.{Config, ConfigException}

import scala.concurrent.{ExecutionContext, Future}

abstract class ServiceLocator(circuitBreakers: CircuitBreakersPanel) {

  implicit def executor: ExecutionContext = ExecutionContext.global

  def locate(name: String): Future[Option[URI]]

  protected final def doServiceCallImpl[T](serviceName: String)(block: URI => Future[T]): Future[Option[T]] = {
    locate(serviceName).flatMap {
      case Some(uri) => block(uri).map(Some.apply)
      case None => Future.successful(None)
    }
  }

  final def doServiceCall[T](serviceName: String, operationId: String)(block: URI => Future[T]): Future[T] = {
    doServiceCallImpl(serviceName) { uri =>
      circuitBreakers.withCircuitBreaker(CircuitBreakerId(serviceName, operationId))(block(uri))
    }.map {
      case Some(result) => result
      case None => throw ServiceLocator.ServiceNotFoundException(serviceName)
    }
  }

}

object ServiceLocator {

  final case class ServiceNotFoundException(name: String) extends RuntimeException

  def config(config: Config, circuitBreakers: CircuitBreakersPanel): ServiceLocator = {
    new ServiceLocator(circuitBreakers) {
      override def locate(name: String): Future[Option[URI]] = {
        Future(Option(config).map(_.getString(s"$name.uri")).map(s => new URI(s))).recover {
          case _: ConfigException.Missing => None
        }
      }
    }
  }

  def static(f: PartialFunction[String, URI], circuitBreakers: CircuitBreakersPanel): ServiceLocator = {
    new ServiceLocator(circuitBreakers) {
      override def locate(name: String): Future[Option[URI]] = Future(f.lift(name))
    }
  }

}
