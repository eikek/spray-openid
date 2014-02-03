package org.eknet.spray.openid.model

import spray.http.Uri

case class CheckIdRequest(ns: String = namespaceOpenId2,
                          mode: String,
                          claimedId: String,
                          identity: String,
                          assocHandle: Option[String],
                          returnTo: Option[String],
                          realm: Option[String],
                          adds: Map[String, String]) {

  def appendToQuery(uri: Uri): Uri = uri.appendToQuery(toMap)

  def toMap = filterNonEmpty(adds ++ Map(
    "openid.ns" -> ns,
    "openid.mode" -> mode,
    "openid.claimed_id" -> claimedId,
    "openid.identity" -> identity,
    "openid.assoc_handle" -> assocHandle.getOrElse(""),
    "openid.return_to" -> returnTo.getOrElse(""),
    "openid.realm" -> realm.getOrElse("")
  ))

  def supplement(more: Map[String, String]) = copy(adds = adds ++ more)

  val isImmediate = mode == "checkid_immediate"

  val returnToMatchesRealm = List(returnTo, realm) match {
    case Some(rto) :: Some(r) :: Nil => uriMatchesRealm(Uri(rto), Uri(r))
    case _ => true
  }
}

object CheckIdRequest {

  def setup(claimedId: String,
            returnTo: String,
            identity: String,
            assocHandle: Option[String],
            realm: Option[String],
            adds: Map[String, String]): CheckIdRequest =
    CheckIdRequest(namespaceOpenId2, "checkid_setup", claimedId, identity, assocHandle, Some(returnTo), realm, adds)

  def immediate(claimedId: String,
                returnTo: String,
                identity: String,
                assocHandle: Option[String],
                realm: Option[String],
                adds: Map[String, String]): CheckIdRequest =
    CheckIdRequest(namespaceOpenId2, "checkid_immediate", claimedId, identity, assocHandle, Some(returnTo), realm, adds)

  def apply(claimedId: String,
            returnTo: String,
            identity: String,
            assocHandle: Option[String],
            realm: Option[String],
            immediatereq: Boolean): CheckIdRequest =
    if (immediatereq) immediate(claimedId, returnTo, identity, assocHandle, realm, Map.empty)
    else setup(claimedId, returnTo, identity, assocHandle, realm, Map.empty)


  private val fieldList = List("openid.ns", "openid.mode", "openid.claimed_id", "openid.identity",
    "openid.assoc_handle", "openid.return_to", "openid.realm")

  implicit val CheckIdReqUnmarshaller = indirectReqUnmarshaller { fields =>
    val fieldSet = fieldList.toSet
    val adds = fields.filterKeys(x => x.startsWith("openid.") && !fieldSet.contains(x))
    fieldList.map(fields.get) match {
      case ns :: Some(mode) :: Some(cid) :: Some(id) :: ah :: rto :: realm :: Nil =>
        CheckIdRequest(ns.getOrElse(namespaceOpenId11), mode, cid, id, ah, rto, realm, adds)
      case _ => sys.error(s"Invalid checkid request: $fields")
    }
  }
}
