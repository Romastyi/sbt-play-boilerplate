package play.boilerplate.api.server.scaldi

import akka.actor.ActorSystem
import play.api.Application
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits
import play.boilerplate.api.server.dsl.HttpErrorHandler
import scaldi.Module

import scala.concurrent.ExecutionContext

class ScaldiPlayModule(app: Application, errorHandler: HttpErrorHandler) extends Module {
  bind [ActorSystem] identifiedBy 'playActorSystem to Akka.system(app) destroyWith (_.shutdown())
  bind [ExecutionContext] identifiedBy 'playExecutionContext to Implicits.defaultContext
  bind [HttpErrorHandler] identifiedBy 'errorHandler to errorHandler
}
