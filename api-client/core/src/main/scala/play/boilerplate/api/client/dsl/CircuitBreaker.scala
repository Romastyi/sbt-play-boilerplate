package play.boilerplate.api.client.dsl

import scala.concurrent.{ExecutionContext, Future}

trait CircuitBreaker {
  def withCircuitBreaker[T](block: => Future[T])(implicit ec: ExecutionContext): Future[T]
}

object CircuitBreaker {
  object None extends CircuitBreaker {
    override def withCircuitBreaker[T](block: => Future[T])(implicit ec: ExecutionContext): Future[T] = block
  }
}
