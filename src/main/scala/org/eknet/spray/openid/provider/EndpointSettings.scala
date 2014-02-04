package org.eknet.spray.openid.provider

import akka.actor.ActorRef
import spray.http.Uri

case class EndpointSettings(endpoint: Uri, hook: ProviderHooks, assocRef: ActorRef)
