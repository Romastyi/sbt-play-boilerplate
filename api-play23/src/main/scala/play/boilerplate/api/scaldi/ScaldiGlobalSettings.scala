package play.boilerplate.api.scaldi

/**
 * Created by romastyi on 17.01.16.
 * Project: Loya
 */

import play.api.Application
import scaldi.{Injector, LifecycleManager, NilInjector}
import scaldi.play.ScaldiSupport

trait ScaldiGlobalSettings extends ScaldiSupport {

  // Scaldi injection support
  private var loadedInjector: Option[Injector with LifecycleManager] = None

  final override def applicationModule: Injector = NilInjector

  override implicit lazy val injector: Injector = super.injector ++ loadedInjector.getOrElse {
    throw new IllegalStateException("No injector found. Is application running?")
  }

  override def onStart(app: Application): Unit = {
    super.onStart(app)
    loadedInjector = Some(ScaldiBuilder.loadModules(app.configuration, app.classloader))
  }

  override def onStop(app: Application): Unit = {
    loadedInjector.foreach(_.destroy())
    loadedInjector = None
    super.onStop(app)
  }

}

object ScaldiGlobalSettings extends ScaldiGlobalSettings