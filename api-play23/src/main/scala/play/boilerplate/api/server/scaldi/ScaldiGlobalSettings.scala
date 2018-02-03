package play.boilerplate.api.server.scaldi

/**
 * Created by romastyi on 17.01.16.
 * Project: Loya
 */

import play.api.Application
import play.api.http.Status._
import play.api.mvc.{Handler, RequestHeader, Result}
import play.boilerplate.api.server.dsl.{HttpErrorHandler, InjectedRoutes}
import scaldi.play.ScaldiSupport
import scaldi.{Injector, LifecycleManager, NilInjector}

import scala.concurrent.Future

trait ScaldiGlobalSettings extends ScaldiSupport {

  // Scaldi injection support
  private var loadedInjector: Option[Injector with LifecycleManager] = None

  final override def applicationModule: Injector = NilInjector

  override implicit lazy val injector: Injector = super.injector ++ loadedInjector.getOrElse {
    throw new IllegalStateException("No injector found. Is application running?")
  }

  private def createInjector(currentApplication: Application, errorHandler: HttpErrorHandler): Injector with LifecycleManager = {
    ScaldiBuilder.loadModules(currentApplication.configuration, currentApplication.classloader) ::
      new ScaldiPlayModule(currentApplication, errorHandler)
  }

  override def onStart(app: Application): Unit = {
    super.onStart(app)
    httpErrorHandler = HttpErrorHandler.loadFromConfiguration(app.configuration, app.classloader)
    loadedInjector = Some(createInjector(app, errorHandler))
  }

  override def onStop(app: Application): Unit = {
    loadedInjector.foreach(_.destroy())
    loadedInjector = None
    super.onStop(app)
  }

  // Routes injection support
  private lazy val routes: Seq[InjectedRoutes] = injectAllOfType[InjectedRoutes]

  protected def handlerFor(request: RequestHeader): Option[Handler] = {
    if (routes.isEmpty) {
      None
    } else {
      routes.map(_.routes.routes).reduceLeft(_ orElse _).lift(request)
    }
  }

  override def onRouteRequest(request: RequestHeader): Option[Handler] = {
    super.onRouteRequest(request) orElse handlerFor(request)
  }

  // HttpErrorHandler
  private var httpErrorHandler: Option[HttpErrorHandler] = None

  private val defaultHttpErrorHandler = HttpErrorHandler.defaultHttpErrorHandler(this)

  def errorHandler: HttpErrorHandler = httpErrorHandler.getOrElse(defaultHttpErrorHandler)

  override def onBadRequest(request: RequestHeader, error: String): Future[Result] = {
    errorHandler.onClientError(request, BAD_REQUEST, error)
  }

  override def onHandlerNotFound(request: RequestHeader): Future[Result] = {
    errorHandler.onClientError(request, NOT_FOUND)
  }

  override def onError(request: RequestHeader, ex: Throwable): Future[Result] = {
    errorHandler.onServerError(request, ex)
  }

}

object ScaldiGlobalSettings extends ScaldiGlobalSettings