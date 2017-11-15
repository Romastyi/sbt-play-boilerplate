package play.boilerplate.utils

final case class CircuitBreakerId(serviceName: String, operationId: String) {
  def id: String = s"$serviceName-$operationId"
}
