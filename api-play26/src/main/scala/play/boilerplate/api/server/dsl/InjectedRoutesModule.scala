package play.boilerplate.api.server.dsl

import play.api.http.HttpConfiguration
import play.api.inject.RoutesProvider
import play.api.routing.Router
import scaldi.Module

final case class InjectedRoutes(routes: Compat.Routes)

final class InjectedRoutesModule extends Module {
  bind [Router] to {
    val httpConfig = inject[HttpConfiguration]
    val routes = new InjectedRouter(Router.empty)(injectAllOfType[InjectedRoutes])
      .withPrefix(httpConfig.context)
    new InjectedRouter(inject[RoutesProvider].get)(InjectedRoutes(routes) :: Nil)
  }
}

private class InjectedRouter(fallback: Router)(injected: Seq[InjectedRoutes]) extends Router {

  override val routes: Router.Routes = {
    (injected.map(_.routes) :+ fallback).map(_.routes).reduceLeft(_ orElse _)
  }

  override def documentation: Seq[(String, String, String)] = {
    injected.flatMap(_.routes.documentation) ++ fallback.documentation
  }

  override def withPrefix(prefix: String): Router = {
    new InjectedRouter(fallback.withPrefix(prefix))(injected.map(
      router => router.copy(routes = router.routes.withPrefix(prefix))
    ))
  }

}