package play.boilerplate.api.client.dsl

/**
  * Based on https://github.com/jboner/lagom-service-locator-consul
  */

import com.typesafe.config.Config

final case class ConsulConfig(agentHostname: String, agentPort: Int, scheme: String, routingPolicy: RoutingPolicy)

object ConsulConfig {

  def fromConfig(config: Config): ConsulConfig = ConsulConfig(
    agentHostname = config.getString("consul.agent-hostname"),
    agentPort = config.getInt("consul.agent-port"),
    scheme = config.getString("consul.uri-scheme"),
    routingPolicy = RoutingPolicy(config.getString("consul.routing-policy"))
  )

}