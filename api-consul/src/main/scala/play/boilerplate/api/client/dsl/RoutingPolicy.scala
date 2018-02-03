package play.boilerplate.api.client.dsl

import com.typesafe.config.ConfigException.BadValue

sealed trait RoutingPolicy

object RoutingPolicy {

  case object First extends RoutingPolicy
  case object Random extends RoutingPolicy
  case object RoundRobin extends RoutingPolicy

  def apply(name: String): RoutingPolicy = name.toLowerCase match {
    case "first" => First
    case "random" => Random
    case "round-robin" => RoundRobin
    case unknown =>
      throw new BadValue("routing-policy", s"[$unknown] is not a valid routing algorithm")
  }

}
