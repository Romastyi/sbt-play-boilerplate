package play.boilerplate.api.client.dsl

final case class CircuitBreakerId(serviceName: String, operationId: String) {
  def id: String = s"$serviceName.$operationId"
}
