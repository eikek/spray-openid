package org.eknet.spray.openid.model

case class CheckAuthenticationResponse(ns: String, valid: Boolean, invalidateHandle: Option[String]) extends DirectResponse

object CheckAuthenticationResponse {
  private val fieldList = List("ns", "is_valid", "invalidate_handle")

  implicit val checkAuthResponseUnmarshaller = directResponseUnmarshaller { fields =>
    fieldList.map(fields.get) match {
      case ns :: Some(isvalid) :: invh :: Nil =>
        CheckAuthenticationResponse(ns.getOrElse(namespaceOpenId11), isvalid.toBoolean, invh)
      case _ => sys.error("Invalid check-authentication response")
    }
  }
}