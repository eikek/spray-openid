package org.eknet.spray.openid

import spray.routing._
import akka.actor.ActorSystem
import akka.util.Timeout
import org.eknet.spray.openid.provider._
import org.eknet.spray.openid.provider.EndpointSettings
import spray.http.{HttpCookie, Uri}

object Provider extends SimpleRoutingApp with Directives with App {

  implicit val system = ActorSystem("openid-provider")
  implicit val timeout = Timeout(4000)
  import system.dispatcher

  // actor keeping track of associations
  val assocRef = system.actorOf(AssociationActor(), name = "openid-assoc")

  //meaning, an OpenId identifier is someting like 'http://localhost:8888/john'
  val identityBase = "http://localhost:8888/"

  //here the user is authenticated if the password equals the username
  val hooks = ProviderHooks(AccountIdentity.urlSuffix(identityBase)) {
    val fromForm: Directive1[String] = formField("spray-openid.username").flatMap { un =>
      formField("spray-openid.password").flatMap { pw: String =>
        if (un == pw) {
          setCookie(HttpCookie("SPRAY_OPENID_PROV", un)).hflatMap(_ => provide(un))
        }
        else reject()
      }
    }
    val fromCookie: Directive1[String] = cookie("SPRAY_OPENID_PROV").flatMap(c => provide(c.content))
    fromForm | fromCookie
  }

  val discoverySetting = DiscoverySettings.forPathPrefix(Uri(identityBase))
  val endpointSetting = EndpointSettings(discoverySetting.endpointUrl, hooks, assocRef)
  val provider = new ProviderRoute(endpointSetting, discoverySetting)

  startServer("localhost", 8888) {
    provider.route
  }
}
