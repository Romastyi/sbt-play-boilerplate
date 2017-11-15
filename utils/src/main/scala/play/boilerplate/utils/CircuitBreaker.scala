package play.boilerplate.utils

import scala.concurrent.Future

trait CircuitBreaker {
  def withCircuitBreaker[T](block: => Future[T]): Future[T]
}

object CircuitBreaker {
  object None extends CircuitBreaker {
    override def withCircuitBreaker[T](block: => Future[T]): Future[T] = block
  }
}
