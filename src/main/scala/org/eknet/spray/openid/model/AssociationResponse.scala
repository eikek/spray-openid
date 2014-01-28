package org.eknet.spray.openid.model

case class AssociationResponse(ns: String,
                               assocHandle: String,
                               sessionType: String,
                               assocType: String,
                               expires: Int,
                               macKey: String,
                               pubKey: Option[String]) extends DirectResponse

object AssociationResponse {
  val valueList = List("ns", "assoc_handle", "session_type", "assoc_type", "expires_in", "mac_key", "enc_mac_key", "dh_server_public")
  implicit val AssociationRespUnmarshaller = directResponseUnmarshaller[AssociationResponse] { kv =>
    valueList.map(kv.get) match {
      case ns :: Some(ah) :: Some(st) :: Some(at) :: Some(exp) :: mac :: encmac :: pk :: Nil =>
        mac match {
          case Some(m) => AssociationResponse(ns.getOrElse(namespaceOpenId11), ah, st, at, exp.toInt, m, pk)
          case _ => AssociationResponse(ns.getOrElse(namespaceOpenId11), ah, st, at, exp.toInt, encmac.get, pk)
        }
      case _ => sys.error("Invalid association response: "+ kv)
    }
  }
}
