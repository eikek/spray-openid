package org.eknet.spray.openid.model

import spray.httpx.marshalling.Marshaller
import spray.http.FormData

case class CheckAuthenticationRequest(fields: Map[String, String]) extends DirectRequest {
  def mode = "check_authentication"
}

object CheckAuthenticationRequest {

  implicit val checkAuthRequestMarshaller =
    Marshaller.delegate[CheckAuthenticationRequest, FormData](`form-urlencoded`)(req => {
      val params = req.fields.updated("openid.mode", req.mode).updated("openid.ns", req.ns)
      FormData(params)
    })
}
