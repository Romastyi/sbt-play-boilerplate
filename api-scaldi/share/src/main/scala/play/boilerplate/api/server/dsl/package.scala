package play.boilerplate.api.server

import play.api.routing.Router

// For versions compatibility
package object dsl {
  type InjectedRoutes = scaldi.play.InjectedRoutes
  object InjectedRoutes {
    def apply(routes: Router): InjectedRoutes = scaldi.play.InjectedRoutes(routes)
  }
}
