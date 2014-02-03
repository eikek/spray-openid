package org.eknet.spray.openid.provider

import spray.http.Uri
import org.eknet.spray.openid.model.Discovery._

object XrdsDocument {
  sealed abstract class XrdsType(val name: String)
  case object Signon extends XrdsType("signon")
  case object Server extends XrdsType("server")

  def apply(endpoint: Uri, xrdsType: XrdsDocument.XrdsType): ServiceList = {
    val elem = xrdsType match {
      case Server => OpElement(endpoint.toString(), List(ClaimedIdElement.version), 0)
      case Signon => ClaimedIdElement(endpoint.toString(), None, List(ClaimedIdElement.version), 0)
    }
    ServiceList("", List(elem))
  }
}
