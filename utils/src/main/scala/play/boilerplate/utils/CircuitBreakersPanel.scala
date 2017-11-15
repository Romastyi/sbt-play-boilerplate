package play.boilerplate.utils

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.function.{Function => JFunction}

import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

trait CircuitBreakersPanel {
  def withCircuitBreaker[T](id: CircuitBreakerId)
                           (block: => Future[T])
                           (implicit ec: ExecutionContext): Future[T]
}

object CircuitBreakersPanel {

  val defaultConfig: Config = ConfigFactory.parseString(
    """default {
      |  # Possibility to disable a given circuit breaker.
      |  enabled = on
      |
      |  # Number of failures before opening the circuit.
      |  max-failures = 10
      |
      |  # Duration of time after which to consider a call a failure.
      |  call-timeout = 10s
      |
      |  # Duration of time in open state after which to attempt to close
      |  # the circuit, by first entering the half-open state.
      |  reset-timeout = 15s
      |}""".stripMargin
  )

  object Without extends CircuitBreakersPanel {
    override def withCircuitBreaker[T](id: CircuitBreakerId)
                                      (block: => Future[T])
                                      (implicit ec: ExecutionContext): Future[T] = block
  }

  /**
    * Based on com.lightbend.lagom.internal.client.CircuitBreakersPanelInternal
    */
  abstract class DefaultImpl(config: Config) extends CircuitBreakersPanel {

    private val breakersByServices = new ConcurrentHashMap[String, Option[CircuitBreaker]]
    private val breakersByOperations = new ConcurrentHashMap[String, Option[CircuitBreaker]]

    val effectiveConfig: Config = Option(config).map(_.withFallback(defaultConfig)).getOrElse(defaultConfig)
    val defaultBreakerConfig: Config = effectiveConfig.getConfig("default")

    def createCircuitBreaker(breakerConfig: CircuitBreakerConfig)(implicit ec: ExecutionContext): CircuitBreaker

    final override def withCircuitBreaker[T](id: CircuitBreakerId)
                                            (block: => Future[T])
                                            (implicit ec: ExecutionContext): Future[T] = {
      breaker(id) match {
        case Some(circuitBreaker) =>
          circuitBreaker.withCircuitBreaker(block)
        case None =>
          block
      }
    }

    private def configCircuitBreaker(defaultIfAbsent: Boolean)(implicit ec: ExecutionContext) = {
      new JFunction[String, Option[CircuitBreaker]] {
        override def apply(id: String): Option[CircuitBreaker] = {
          if (config.hasPath(id) || defaultIfAbsent) {
            val breakerConfig =
              if (config.hasPath(id)) config.getConfig(id).withFallback(defaultBreakerConfig)
              else defaultBreakerConfig
            if (breakerConfig.getBoolean("enabled")) {
              val breaker = createCircuitBreaker(CircuitBreakerConfig(
                maxFailures = breakerConfig.getInt("max-failures"),
                callTimeout = breakerConfig.getDuration("call-timeout", MILLISECONDS).millis,
                resetTimeout = breakerConfig.getDuration("reset-timeout", MILLISECONDS).millis
              ))
              Some(breaker)
            } else {
              Some(CircuitBreaker.None)
            }
          } else {
            None
          }
        }
      }
    }

    private def breaker(id: CircuitBreakerId)(implicit ec: ExecutionContext): Option[CircuitBreaker] = {
      breakersByOperations.computeIfAbsent(id.id, configCircuitBreaker(defaultIfAbsent = false)) orElse
        breakersByServices.computeIfAbsent(id.serviceName, configCircuitBreaker(defaultIfAbsent = true))
    }

  }

}
