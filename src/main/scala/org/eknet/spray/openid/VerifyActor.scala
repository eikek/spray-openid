package org.eknet.spray.openid

import akka.actor.{Props, ActorLogging, Actor}
import org.eknet.spray.openid.model._
import spray.http.{DateTime, HttpResponse, HttpRequest, Uri}
import org.eknet.spray.openid.model.PositiveAssertion.ResponseNonce
import org.eknet.spray.openid.model.AssociationRequest.SessionType
import scala.concurrent.Future
import org.parboiled.common.Base64
import spray.http.HttpRequest
import scala.Some

object VerifyActor {
  def apply() = Props(classOf[VerifyActor])

  case class VerifyAssertion(req: Uri, pos: PositiveAssertion)
  case class VerifyResult(result: Boolean)

  case class Associate(uri: Uri)
  case class AssociateResult(handle: Option[String])

  case class Assoc(resp: AssociationResponse, keypair: Crypt.DH.DHKeyPair, modulus: BigInt, gen: BigInt) {
    import scala.concurrent.duration._
    private val valid = System.currentTimeMillis() + resp.expires.seconds.toMillis
    def isValid = valid > System.currentTimeMillis()
    def serverPubkey = resp.pubKey.map(pk => BigInt(Base64.rfc2045().decode(pk)))
    lazy val sessionType = resp.sessionType match {
      case "DH-SHA1" => SessionType.sha1(keypair.getPublic.getY, modulus, gen)
      case "DH-SHA256" => SessionType.sha256(keypair.getPublic.getY, modulus, gen)
      case "no-encryption" => SessionType.none
      case _ => sys.error("Invalid session type name: "+ resp.sessionType)
    }
    lazy val assocHmac = resp.assocType match {
      case "HMAC-SHA1" => Crypt.HmacSha1
      case "HMAC-SHA256" => Crypt.HmacSha256
      case _ => sys.error("Invalid assoc_type value: "+ resp.assocType)
    }
  }

  private case class RegisterAssoc(handle: String, assoc: Assoc)
  private case class RemoveAssoc(handle: String)
  private case class RemoveNonce(nonce: ResponseNonce)
}

/**
 * Actor keeping track of associations and response nonces.
 *
 */
class VerifyActor extends Actor with ActorLogging {
  import VerifyActor._
  import spray.client.pipelining._
  import akka.pattern.pipe
  import scala.concurrent.duration._
  import context.dispatcher

  //todo make delay configurable
  private val cacheTimeout = 5.minutes
  private var assocs = Map.empty[String, Assoc]
  private var nonces = Set.empty[ResponseNonce]

  def receive = {
    case Associate(provider) =>
      val (modulus, gen) = (Crypt.DH.defaultModulus, Crypt.DH.defaultG)
      val keypair = Crypt.DH.generateKeyPair(Crypt.DH.parameterSpec(modulus, gen)).get
      val session = SessionType.sha256(BigInt(keypair.getPublic.getY), modulus, gen)
      val req = AssociationRequest(session)
      val f = assocPipeline(Post(provider, req)).map { resp =>
        self ! RegisterAssoc(resp.assocHandle, Assoc(resp, keypair, modulus, gen))
        log.debug(s"Successful associated with $provider: $resp")
        AssociateResult(Some(resp.assocHandle))
      } recover {
        case ex =>
          log.error(ex, "Error during association")
          AssociateResult(None)
      }
      f pipeTo sender

    case RegisterAssoc(handle, assoc) =>
      assocs = assocs.updated(handle, assoc)
      context.system.scheduler.scheduleOnce(cacheTimeout, self, RemoveAssoc(handle))

    case RemoveAssoc(handle) =>
      assocs = assocs - handle

    case RemoveNonce(nonce) =>
      nonces = nonces - nonce

    case VerifyAssertion(uri, pos) =>
      val assoc = assocs.get(pos.assocHandle)
      val f = Verification.verifyAll(uri, nonces, cacheTimeout, assoc)(pos) match {
        case Left(msg) =>
          log.error("Verification of OpenId assertion failed: "+ msg)
          Future.successful(VerifyResult(result = false))
        case Right(_) =>
          if (assoc.isEmpty) {
            log.warning("No association found. Cannot verify signature locally; Asking provider for verification")
            val f = checkAuthFuture(pos)
            f.map(r => VerifyResult(r.valid)).recover({ case x => VerifyResult(result = false)})
          } else {
            log.info("Positive assertion successfully verified")
            Future.successful(VerifyResult(result = true))
          }
      }
      nonces = nonces + pos.nonce
      context.system.scheduler.scheduleOnce(cacheTimeout, self, RemoveNonce(pos.nonce))
      self ! RemoveAssoc(pos.assocHandle)
      f pipeTo sender

  }

  private val assocPipeline: HttpRequest => Future[AssociationResponse] =
    sendReceive ~> unmarshal[AssociationResponse]

  private val checkAuthPipeline: HttpRequest => Future[CheckAuthenticationResponse] =
    sendReceive ~> unmarshal[CheckAuthenticationResponse]

  private def checkAuthFuture(pos: PositiveAssertion) = {
    val f = checkAuthPipeline(Post(pos.opEndpoint, pos.toCheckAuthentication))
    f.onSuccess {
      case CheckAuthenticationResponse(_, true, _) => log.info("Provider verified authentication assertion")
    }
    f.onFailure {
      case ex => log.error(ex, "Provider did not verify authentication assertion.")
    }
    f
  }
}
