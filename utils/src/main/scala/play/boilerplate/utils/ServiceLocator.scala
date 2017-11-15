package play.boilerplate.utils

import java.net.URI

import com.typesafe.config.{Config, ConfigException}

import scala.concurrent.{ExecutionContext, Future}

abstract class ServiceLocator(circuitBreakers: CircuitBreakersPanel) {

  def locate(name: String): Future[Option[URI]]

  protected final def doServiceCallImpl[T](serviceName: String)
                                          (block: URI => Future[T])
                                          (implicit ec: ExecutionContext): Future[Option[T]] = {
    locate(serviceName).flatMap {
      case Some(uri) => block(uri).map(Some.apply)
      case None => Future.successful(None)
    }
  }

  final def doServiceCall[T](serviceName: String, operationId: String)
                            (block: URI => Future[T])
                            (implicit ec: ExecutionContext): Future[T] = {
    doServiceCallImpl(serviceName) { uri =>
      circuitBreakers.withCircuitBreaker(CircuitBreakerId(serviceName, operationId))(block(uri))
    }.map {
      case Some(result) => result
      case None => throw ServiceLocator.ServiceNotFoundException(serviceName)
    }
  }

}

object ServiceLocator {

  import ExecutionContext.Implicits.global

  final case class ServiceNotFoundException(name: String) extends RuntimeException

  case class DefaultImpl(config: Config)
                        (implicit cb: CircuitBreakersPanel = CircuitBreakersPanel.Without)
    extends ServiceLocator(cb) {
    override def locate(name: String): Future[Option[URI]] = {
      Future(Option(config).map(_.getString(s"$name.uri")).map(s => new URI(s))).recover {
        case _: ConfigException.Missing => None
      }
    }
  }

  case class Static(resolver: PartialFunction[String, URI])
                   (implicit cb: CircuitBreakersPanel = CircuitBreakersPanel.Without)
    extends ServiceLocator(cb) {
    override def locate(name: String): Future[Option[URI]] = Future(resolver.lift(name))
  }

}
