package play.boilerplate.utils

import scala.concurrent.Future

trait CircuitBreaker {
  def withCircuitBreaker[T](block: => Future[T]): Future[T]
}

