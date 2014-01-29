package org.eknet.spray.openid.model

import spray.http.Uri

case class CheckIdRequest(ns: String = namespaceOpenId2,
                          mode: String,
                          claimedId: String,
                          identity: Option[String],
                          assocHandle: Option[String],
                          returnTo: Option[String],
                          realm: Option[String],
                          adds: Map[String, String]) {

  def appendToQuery(uri: Uri): Uri =
    uri.withQuery(uri.query.toMap ++ filterNonEmpty(adds ++ Map(
      "openid.ns" -> ns,
      "openid.mode" -> mode,
      "openid.claimed_id" -> claimedId,
      "openid.identity" -> identity.getOrElse(claimedId),
      "openid.assoc_handle" -> assocHandle.getOrElse(""),
      "openid.return_to" -> returnTo.getOrElse(""),
      "openid.realm" -> realm.getOrElse("")
    )))

  def supplement(adds: Map[String, String]) = copy(adds = adds)
}

object CheckIdRequest {

  def setup(claimedId: String,
            returnTo: String,
            identity: Option[String],
            assocHandle: Option[String],
            realm: Option[String],
            adds: Map[String, String]): CheckIdRequest =
    CheckIdRequest(namespaceOpenId2, "checkid_setup", claimedId, identity, assocHandle, Some(returnTo), realm, adds)

  def immediate(claimedId: String,
                returnTo: String,
                identity: Option[String],
                assocHandle: Option[String],
                realm: Option[String],
                adds: Map[String, String]): CheckIdRequest =
    CheckIdRequest(namespaceOpenId2, "checkid_immediate", claimedId, identity, assocHandle, Some(returnTo), realm, adds)

  def apply(claimedId: String,
            returnTo: String,
            identity: Option[String],
            assocHandle: Option[String],
            realm: Option[String],
            adds: Map[String, String],
            immediatereq: Boolean): CheckIdRequest =
    if (immediatereq) immediate(claimedId, returnTo, identity, assocHandle, realm, adds)
    else setup(claimedId, returnTo, identity, assocHandle, realm, adds)

}
