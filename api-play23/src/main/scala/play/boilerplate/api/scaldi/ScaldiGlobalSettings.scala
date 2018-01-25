package play.boilerplate.api.scaldi

/**
 * Created by romastyi on 17.01.16.
 * Project: Loya
 */

import play.api.Application
import play.api.mvc.{Handler, RequestHeader}
import play.boilerplate.api.server.dsl.InjectedRoutes
import scaldi.{Injector, LifecycleManager, NilInjector}
import scaldi.play.ScaldiSupport

trait ScaldiGlobalSettings extends ScaldiSupport {

  // Scaldi injection support
  private var loadedInjector: Option[Injector with LifecycleManager] = None

  final override def applicationModule: Injector = NilInjector

  override implicit lazy val injector: Injector = super.injector ++ loadedInjector.getOrElse {
    throw new IllegalStateException("No injector found. Is application running?")
  }

  private def createInjector(currentApplication: Application): Injector with LifecycleManager = {
    ScaldiBuilder.loadModules(currentApplication.configuration, currentApplication.classloader) ::
      new ScaldiPlayModule(currentApplication)
  }

  override def onStart(app: Application): Unit = {
    super.onStart(app)
    loadedInjector = Some(createInjector(app))
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

}

object ScaldiGlobalSettings extends ScaldiGlobalSettings