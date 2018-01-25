package play.boilerplate.api.scaldi

import akka.actor.ActorSystem
import play.api.Application
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits
import scaldi.Module

import scala.concurrent.ExecutionContext

class ScaldiPlayModule(app: Application) extends Module {
  bind [ActorSystem] identifiedBy 'playActorSystem to Akka.system(app)
  bind [ExecutionContext] identifiedBy 'playExecutionContext to Implicits.defaultContext
}
