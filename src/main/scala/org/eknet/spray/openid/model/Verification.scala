package org.eknet.spray.openid.model

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Failure, Success}
import akka.actor.ActorRefFactory
import spray.http.{Uri, DateTime}
import org.eknet.spray.openid.model.PositiveAssertion.ResponseNonce
import org.eknet.spray.openid.model.Discovery._

object Verification {

  type Verifyer = PositiveAssertion => Either[String, PositiveAssertion]

  private def error(msg: String) = Left(msg)

  def verifyAll(uri: Uri, set: Set[ResponseNonce], timeout: FiniteDuration, assoc: Option[Association]): Verifyer = {
    val all = List(nonceTimeout(timeout), nonceOnetime(set),
      returnToRequest(uri), SimpleRegistration.verify(_)) ::: assoc.map(verifySignature).toList
    pos => {
      val el: Either[String, PositiveAssertion] = Right(pos)
      all.foldLeft(el) { (res, next) => res.right.flatMap(next) }
    }
  }

  def nonceTimeout(timeout: FiniteDuration): Verifyer = pos => {
    if (DateTime.now > (pos.nonce.time + timeout.toMillis)) error(s"Nonce '${pos.nonce}' is too old")
    else Right(pos)
  }

  def nonceOnetime(set: Set[ResponseNonce]): Verifyer = pos => {
    if (set contains pos.nonce) error(s"Nonce '${pos.nonce}' alread used!")
    else Right(pos)
  }

  def returnToRequest(req: Uri): Verifyer = pos => {
    val rto = Uri(pos.returnTo)
    val valid = (rto.withQuery("") == req.withQuery("")) &&
      rto.query.toMap.forall { case (key, value) => req.query.get(key) == Some(value) }
    if (valid) Right(pos) else error(s"ReturnTo url does not match request!")
  }

  def verifySignature(assoc: Association): Verifyer = pos => {
    val minimum = Set("op_endpoint", "return_to", "response_nonce", "assoc_handle", "claimed_id", "identity")
    if (!assoc.isValid) {
      error("Association response is not valid anymore.")
    }
    else if (!minimum.subsetOf(pos.signed.toSet)) {
      error("Missing fields in signature: "+ minimum.diff(pos.signed.toSet))
    }
    else {
      val key = assoc.cryptedMacKey.getOrElse(assoc.plainMacKey)
      Crypt.verifySig(key)(pos.sig.decodeBase64, pos.signaturePayload) match {
        case Success(true) => Right(pos)
        case Success(false) => error(s"Signatures do not match.")
        case Failure(ex) => error(s"Error verifying signatures! ${ex.getMessage}")
      }
    }
  }

  def verifyDiscovery(pos: PositiveAssertion)(implicit refFactory: ActorRefFactory, ec: ExecutionContext): Future[Boolean] = {
    def check(providerUri: String, localId: Option[String]) =
      pos.opEndpoint == providerUri && localId.getOrElse(pos.claimedId) == pos.identity

    discover(pos.claimedId).map {
      case ServiceList(_, elems) =>
        elems.exists {
          case ClaimedIdElement(uri, localid, _, _) => check(uri, localid)
          case _ => false
        }
      case HtmlElement(providerUri, localId) => check(providerUri, localId)
    }
  }
}
