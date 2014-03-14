package org.eknet.spray.openid.model

import spray.httpx.marshalling.Marshaller
import spray.http.FormData
import spray.http.MediaTypes._

case class CheckAuthenticationRequest(pos: PositiveAssertion) extends DirectRequest {
  def mode = "check_authentication"

  val signed = pos.signed
  val assocHandle = pos.assocHandle
  val sig = pos.sig
  val signaturePayload: Array[Byte] = pos.signaturePayload

}

object CheckAuthenticationRequest {

  implicit val checkAuthRequestMarshaller =
    Marshaller.delegate[CheckAuthenticationRequest, FormData](`application/x-www-form-urlencoded`)(req => {
      val params = req.pos.fields.updated("openid.mode", req.mode).updated("openid.ns", req.ns)
      FormData(params)
    })

  implicit val checkAuthRequestUnmarshaller =
    directReqUnmarshaller[CheckAuthenticationRequest] { fields =>
      PositiveAssertion(fields) match {
        case Some(pa) => CheckAuthenticationRequest(pa)
        case _ => sys.error("Invalid check-authentication request")
      }
    }
}
