package play.boilerplate.api.client.dsl

import scala.concurrent.duration.FiniteDuration

final case class CircuitBreakerConfig(maxFailures: Int, callTimeout: FiniteDuration, resetTimeout: FiniteDuration)