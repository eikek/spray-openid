package org.eknet.spray.openid.provider

import spray.routing._

class DiscoveryRoute(settings: DiscoverySettings) extends ProviderDirectives {

  def route: Route = {
    settings.xrdsServer {
      complete(XrdsDocument(settings.endpointUrl, XrdsDocument.Server))
    } ~
    settings.identityPath {
      complete(XrdsDocument(settings.endpointUrl, XrdsDocument.Signon))
    }
  }
}
