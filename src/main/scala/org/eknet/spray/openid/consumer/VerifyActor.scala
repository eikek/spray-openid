package org.eknet.spray.openid.consumer

import akka.actor.{Props, ActorLogging, Actor}
import org.eknet.spray.openid.model._
import spray.http.Uri
import org.eknet.spray.openid.model.PositiveAssertion.ResponseNonce
import org.eknet.spray.openid.model.AssociationRequest.{AssocType, SessionType}
import scala.concurrent.Future
import spray.http.HttpRequest

object VerifyActor {
  def apply() = Props(classOf[VerifyActor])

  case class VerifyAssertion(req: Uri, pos: PositiveAssertion)
  case class VerifyResult(result: Boolean)

  case class Associate(uri: Uri)
  case class AssociateResult(handle: Option[String])

  private case class RegisterAssoc(handle: String, assoc: Association)
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
  private var assocs = Map.empty[String, Association]
  private var nonces = Set.empty[ResponseNonce]

  def receive = {
    case Associate(provider) =>
      val (modulus, gen) = (Crypt.DH.defaultModulus, Crypt.DH.defaultG)
      val keypair = Crypt.DH.generateKeyPair(Crypt.DH.parameterSpec(modulus, gen)).get
      val req = AssociationRequest(SessionType.sha256(BigInt(keypair.getPublic.getY), modulus, gen))
      val f = assocPipeline(Post(provider, req)).map { resp =>
        self ! RegisterAssoc(resp.assocHandle, createAssociation(resp, keypair))
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
      val f1 = Verification.verifyAll(uri, nonces, cacheTimeout, assoc)(pos) match {
        case Left(msg) =>
          log.debug("Verification of OpenId assertion failed: "+ msg)
          Future.successful(VerifyResult(result = false))
        case Right(_) =>
          if (assoc.isEmpty) {
            log.warning("No association found. Cannot verify signature locally; Asking provider for verification")
            val f = checkAuthFuture(pos)
            f.map(r => VerifyResult(r.valid)).recover({ case x => VerifyResult(result = false)})
          } else {
            Future.successful(VerifyResult(result = true))
          }
      }
      val f2 = Verification.verifyDiscovery(pos).map(VerifyResult.apply)
      f2.onSuccess {
        case VerifyResult(false) =>
          log.error(s"Discovered information for '${pos.claimedId}' did not match positive assertion.")
      }
      val fr = for (r1 <- f1; r2 <- f2) yield VerifyResult(r1.result && r2.result)
      fr.onSuccess {
        case VerifyResult(true) => log.info("Positive assertion successfully verified")
        case VerifyResult(false) => log.error("Verification of OpenId assertion failed")
      }
      nonces = nonces + pos.nonce
      context.system.scheduler.scheduleOnce(cacheTimeout, self, RemoveNonce(pos.nonce))
      self ! RemoveAssoc(pos.assocHandle)
      fr pipeTo sender

  }

  private def createAssociation(resp: AssociationResponse, keypair: Crypt.DH.DHKeyPair) = {
    val atype = AssocType.fromString(resp.assocType)
    val stype = resp.pubKey.map(pb => SessionType.sha256(pb, Crypt.DH.defaultModulusBase64, Crypt.DH.defaultGBase64))
    Association(atype, stype.getOrElse(SessionType.none), cacheTimeout,
      resp.macKey.decodeBase64.toVector, stype.map(_ => keypair))
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
