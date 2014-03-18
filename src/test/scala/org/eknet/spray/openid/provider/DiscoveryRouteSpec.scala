package org.eknet.spray.openid.provider

import spray.routing.HttpService
import spray.testkit.Specs2RouteTest
import org.specs2.mutable.Specification
import spray.http._
import org.eknet.spray.openid.model._
import org.eknet.spray.openid.model.Discovery.{ClaimedIdElement, OpElement, Discovered, ServiceList}

class DiscoveryRouteSpec extends Specification with Specs2RouteTest with HttpService {
  implicit def actorRefFactory = system

  val settings = DiscoverySettings.forPathPrefix(Uri("http://localhost"))
  val discovery = new DiscoveryRoute(settings).route
  implicit val discoveredUnmarshaller = Discovered.responseUnmarshaller("localhost/john")

  "The Discovery Service" should {

    "return xrds op element" in {
      Get("/openid/auth") ~> discovery ~> check {
        status === StatusCodes.OK
        contentType === ContentType(`application/xrds+xml`, HttpCharsets.`UTF-8`)
        val serviceList = responseAs[Discovered].asInstanceOf[ServiceList]

        serviceList.providerUri === "http://localhost/openid/endpoint"
        serviceList.services.size === 1
        serviceList.protocol === OpElement.version
      }
    }

    "return xrds identity element" in {
      Get("/john") ~> discovery ~> check {
        status === StatusCodes.OK
        contentType === ContentType(`application/xrds+xml`, HttpCharsets.`UTF-8`)
        val serviceList = responseAs[Discovered].asInstanceOf[ServiceList]

        serviceList.providerUri === "http://localhost/openid/endpoint"
        serviceList.services.size === 1
        serviceList.protocol === ClaimedIdElement.version
      }
    }
  }

}
