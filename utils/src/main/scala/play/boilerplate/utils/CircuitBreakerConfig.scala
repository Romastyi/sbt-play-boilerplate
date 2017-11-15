package play.boilerplate.utils

import scala.concurrent.duration.FiniteDuration

final case class CircuitBreakerConfig(maxFailures: Int, callTimeout: FiniteDuration, resetTimeout: FiniteDuration)