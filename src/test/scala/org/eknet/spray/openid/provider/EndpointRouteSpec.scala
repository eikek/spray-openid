package org.eknet.spray.openid.provider

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.routing.{Route, Directive1, HttpService}
import shapeless.{HNil, ::}
import akka.util.Timeout
import spray.http.StatusCodes
import spray.http.HttpHeaders.Location
import spray.util._
import org.eknet.spray.openid.model._
import org.eknet.spray.openid.model.AssociationRequest.{AssocType, SessionType}
import scala.Some
import shapeless.::

class EndpointRouteSpec extends Specification with Specs2RouteTest with HttpService {
  implicit def actorRefFactory = system
  implicit val timeout = Timeout(500)

  val assocActor = actorRefFactory.actorOf(AssociationActor())
  val authDirective: Directive1[String] = new Directive1[String] {
    override def happly(f: (::[String, HNil]) => Route) = f("john" :: HNil)
  }
  val hooks = ProviderHooks(AccountIdentity.urlSuffix("http://localhost"))(authDirective)
  private val ep = "/openid/endpoint"
  val settings = EndpointSettings("http://localhost" + ep, hooks, assocActor)

  val route = new EndpointRoute(settings).route

  private val claimedId = "http://localhost/john"


  "The OpenID endpoint" should {
    "send negative assertion on cancel" in {
      val req = checkIdSetup().supplement(Map("spray-openid.submitType" -> "cancel"))
      Post(ep, req) ~> route ~> check {
        status === StatusCodes.Found
        val uri = response.headers.findByType[Location].get.uri
        uri.query.get("openid.mode") === Some("cancel")
        uri.query.get("openid.ns") === Some(namespaceOpenId2)
        uri.authority.host.address === "return.to"
      }
    }
    "associate with clients without encryption".in {
      val nocrypt = AssociationRequest(SessionType.none)
      Post(ep, nocrypt) ~> route ~> check {
        val resp = responseAs[AssociationResponse]
        assert(resp.assocHandle.length > 0)
        resp.assocType === AssocType.sha256.name
        resp.sessionType === "no-encryption"
        resp.pubKey === None
        resp.expires must be greaterThan 0
        resp.macKey.length must be greaterThan 0
      }
    }
    "associate with clients with sha256 encryption" in {
      val kp = Crypt.DH.generateKeyPair(Crypt.DH.defaultParameter).get
      val st = SessionType.sha256(kp.getPublic.getY, Crypt.DH.defaultModulus, Crypt.DH.defaultG)
      Post(ep, AssociationRequest(st)) ~> route ~> check {
        val resp = responseAs[AssociationResponse]
        assert(resp.assocHandle.length > 0)
        resp.assocType === AssocType.sha256.name
        resp.sessionType === st.name
        resp.pubKey !== None
        resp.expires must be greaterThan 0
        resp.macKey.decodeBase64.length === 32
      }
    }
    "associate with clients with sha1 encryption" in {
      val kp = Crypt.DH.generateKeyPair(Crypt.DH.defaultParameter).get
      val st = SessionType.sha1(kp.getPublic.getY, Crypt.DH.defaultModulus, Crypt.DH.defaultG)
      Post(ep, AssociationRequest(st)) ~> route ~> check {
        val resp = responseAs[AssociationResponse]
        assert(resp.assocHandle.length > 0)
        resp.assocType === AssocType.sha1.name
        resp.sessionType === st.name
        resp.pubKey !== None
        resp.expires must be greaterThan 0
        resp.macKey.decodeBase64.length === 20
      }
    }
  }

  def checkIdSetup() =
    CheckIdRequest.setup(claimedId, "http://return.to", claimedId, None, None, Map.empty)
}
