package play.boilerplate.utils

import java.net.URI

import com.typesafe.config.{Config, ConfigException}

import scala.concurrent.{ExecutionContext, Future}

trait ServiceLocator {

  import ExecutionContext.Implicits.global

  def locate(name: String): Future[Option[URI]]

  def doServiceCall[T](name: String)(block: URI => Future[T]): Future[T] = {
    for {
      serviceOpt <- locate(name)
      result <- serviceOpt match {
        case Some(service) => block(service)
        case None => Future.failed(ServiceLocator.ServiceNotFoundException(name))
      }
    } yield result
  }

}

object ServiceLocator {

  import ExecutionContext.Implicits.global

  final case class ServiceNotFoundException(name: String) extends RuntimeException

  def config(config: Config): ServiceLocator = new ServiceLocator {
    override def locate(name: String): Future[Option[URI]] = {
      Future(Option(config.getString(s"$name.uri")).map(s => new URI(s))).recover {
        case _: ConfigException.Missing => None
      }
    }
  }

  def static(f: PartialFunction[String, URI]): ServiceLocator = new ServiceLocator {
    override def locate(name: String): Future[Option[URI]] = Future(f.lift(name))
  }

}
