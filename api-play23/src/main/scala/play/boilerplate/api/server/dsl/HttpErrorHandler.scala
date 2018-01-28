package play.boilerplate.api.server.dsl

import play.api.{Configuration, PlayException}
import play.api.mvc.{RequestHeader, Result}

import scala.concurrent.Future
import scala.reflect.ClassTag

/**
  * Component for handling HTTP errors in Play.
  *
  * @since 2.4.0
  */
trait HttpErrorHandler {

  /**
    * Invoked when a client error occurs, that is, an error in the 4xx series.
    *
    * @param request The request that caused the client error.
    * @param statusCode The error status code.  Must be greater or equal to 400, and less than 500.
    * @param message The error message.
    */
  def onClientError(request: RequestHeader, statusCode: Int, message: String = ""): Future[Result]

  /**
    * Invoked when a server error occurs.
    *
    * @param request The request that triggered the server error.
    * @param exception The server error.
    */
  def onServerError(request: RequestHeader, exception: Throwable): Future[Result]

}

object HttpErrorHandler {

  def loadFromConfiguration(configuration: Configuration, classloader: ClassLoader): Option[HttpErrorHandler] = {

    def loadClass(key: String, defaultClassName: String): Option[Class[_]] = {
      val className = configuration.getString(key).getOrElse(defaultClassName)
      try {
        Some(classloader.loadClass(className))
      } catch {
        case _: ClassNotFoundException => None
        case e: VirtualMachineError => throw e
        case e: ThreadDeath => throw e
        case e: Throwable =>
          throw new PlayException(s"Cannot load $key", s"$key [$className] was not loaded.", e)
      }
    }

    loadClass("play.http.errorHandler", "ErrorHandler").map { clazz =>
      val inst = clazz.newInstance()
      val ct = implicitly[ClassTag[HttpErrorHandler]].runtimeClass
      if (ct.isInstance(inst)) inst.asInstanceOf[HttpErrorHandler]
      else throw new ClassCastException(clazz.getName + " is not an instance of " + ct)
    }

  }

}