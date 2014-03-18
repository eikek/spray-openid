package org.eknet.spray.openid.provider

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import spray.routing._
import spray.routing.directives.PathDirectives

class ProviderRoute(endpointSettings: EndpointSettings, discoverySettings: DiscoverySettings) extends PathDirectives with RouteConcatenation {

  val endpoint = new EndpointRoute(endpointSettings)
  val discovery = new DiscoveryRoute(discoverySettings)

  def route(implicit ec: ExecutionContext, to: Timeout): Route = {
    discoverySettings.endpointPath {
      endpoint.route
    } ~
    discovery.route
  }
}
