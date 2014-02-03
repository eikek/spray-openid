package org.eknet.spray.openid.provider

import akka.actor.ActorRef

case class EndpointSettings(hook: ProviderHooks, assocRef: ActorRef)
