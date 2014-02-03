package org.eknet.spray.openid.model

import spray.httpx.marshalling.Marshaller
import spray.http.FormData

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

  implicit val AssociationResponseMarshaller =
    directResponseMarshaller[AssociationResponse]{ res => filterNonEmpty(Map(
      "ns" -> namespaceOpenId2,
      "assoc_handle" -> res.assocHandle,
      "session_type" -> res.sessionType,
      "assoc_type" -> res.assocType,
      "expires_in" -> s"${res.expires}",
      "mac_key" -> (if (res.pubKey.isDefined) "" else res.macKey),
      "enc_mac_key" -> (if (res.pubKey.isEmpty) "" else res.macKey),
      "dh_server_public" -> res.pubKey.getOrElse("")
    ))}
}
