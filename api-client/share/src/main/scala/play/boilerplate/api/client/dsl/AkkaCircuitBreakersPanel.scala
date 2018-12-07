package play.boilerplate.api.client.dsl

import akka.actor.ActorSystem
import akka.pattern.{CircuitBreaker => AkkaCircuitBreaker}
import com.typesafe.config.Config

import scala.concurrent.{ExecutionContext, Future}

final class AkkaCircuitBreakersPanel(config: Config)
                                    (implicit system: ActorSystem)
  extends CircuitBreakersPanel.DefaultImpl(config) {

  case class CircuitBreakerImpl(breakerConfig: CircuitBreakerConfig)(implicit ec: ExecutionContext) extends CircuitBreaker {

    lazy val breaker = new AkkaCircuitBreaker(
      system.scheduler,
      breakerConfig.maxFailures,
      breakerConfig.callTimeout,
      breakerConfig.resetTimeout
    )

    override def withCircuitBreaker[T](block: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
      breaker.withCircuitBreaker(block)
    }

  }

  override def createCircuitBreaker(breakerConfig: CircuitBreakerConfig)(implicit ec: ExecutionContext): CircuitBreaker = {
    CircuitBreakerImpl(breakerConfig)
  }

}

object AkkaCircuitBreakersPanel {

  def instance(config: Config)(implicit system: ActorSystem): CircuitBreakersPanel = new AkkaCircuitBreakersPanel(config)

}