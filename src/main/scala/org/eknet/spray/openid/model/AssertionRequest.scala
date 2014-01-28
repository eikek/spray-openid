package org.eknet.spray.openid.model

import spray.http.DateTime
import scala.util.Try
import org.eknet.spray.openid.model.PositiveAssertion.ResponseNonce

sealed trait AssertionRequest

case class NegativeAssertion(ns: String = namespaceOpenId2, mode: String) extends AssertionRequest

object NegativeAssertion {

  def cancel(ns: String = namespaceOpenId2) = NegativeAssertion(ns, "cancel")
  def setupNeeded(ns: String = namespaceOpenId2) = NegativeAssertion(ns, "setup_needed")

  val valueList = List("openid.ns", "openid.mode")

  implicit val NegativeAssertionUnmarshaller = indirectReqUnmarshaller[NegativeAssertion] { fields =>
    valueList.map(fields.get) match {
      case ns :: Some(mode) :: Nil if mode == "cancel" || mode == "setup_needed" =>
        NegativeAssertion(ns.getOrElse(namespaceOpenId11), mode)
      case _ => sys.error("Invalid negative assertion: "+ fields)
    }
  }
}

case class PositiveAssertion(ns: String,
                             mode: String,
                             opEndpoint: String,
                             claimedId: String,
                             identity: Option[String],
                             returnTo: String,
                             nonce: ResponseNonce,
                             invalidateHandle: Option[String],
                             assocHandle: String,
                             signed: List[String],
                             sig: String,
                             fields: Map[String, String]) extends AssertionRequest {
  require(opEndpoint.nonEmpty, "OP Endpoint was not provided")
  require(returnTo.nonEmpty, "ReturnTo was not returned from OP")
  require(assocHandle.nonEmpty, "No association-handle was provided")
  require(Set("op_endpoint", "return_to", "response_nonce", "assoc_handle").subsetOf(signed.toSet), "Some parameters were not signed")
  require(sig.nonEmpty, "No signature was provided")
  require(claimedId.nonEmpty, "No claimed_id was provided")

  def toCheckAuthentication = CheckAuthenticationRequest(fields)
}

object PositiveAssertion {
  val valueList = List("openid.ns", "openid.mode", "openid.op_endpoint",
    "openid.claimed_id", "openid.identity", "openid.return_to", "openid.response_nonce",
    "openid.invalidate_handle", "openid.assoc_handle", "openid.signed", "openid.sig")

  case class ResponseNonce(time: DateTime, unique: String) {
    def asString = time.toIsoDateTimeString+"Z"+unique
  }
  object ResponseNonce {
    private val isoLen = "yyyy-mm-ddThh:mm:ss".length
    def fromString(s: String) = Try {
      val ds = s.substring(0, isoLen)
      DateTime.fromIsoDateTimeString(ds) match {
        case Some(dt) => ResponseNonce(dt, s.substring(isoLen +1))
        case _ => sys.error("Invalid responce nonce string: "+ s)
      }
    }
  }
  implicit val PositiveAssertionUnmarshaller = indirectReqUnmarshaller[PositiveAssertion] { fields =>
    valueList.map(fields.get) match {
      case ns::Some("id_res")::Some(ep)::Some(cid)::id::Some(rto)::Some(nonce)::invh::Some(assh)::Some(signed)::Some(sig)::Nil =>
        PositiveAssertion(ns.getOrElse(namespaceOpenId11), "id_res", ep, cid, id, rto,
          ResponseNonce.fromString(nonce).get, invh, assh,
          signed.split(',').map(_.trim).filter(_.nonEmpty).toList, sig, fields)
      case _ => sys.error("Invalid positive assertion response: "+ fields)
    }
  }
}