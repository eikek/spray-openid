package org.eknet.spray.openid.model

case class AssociationUnsupported(ns: String = namespaceOpenId2,
                                  error: String,
                                  errorCode: String,
                                  sessionType: Option[String],
                                  assocType: Option[String]) extends DirectResponse

object AssociationUnsupported {
  val valueList = List("ns", "error", "error_code", "session_type", "assoc_type")

  implicit val AssocUnsupportedUnmarshaller = directResponseUnmarshaller[AssociationUnsupported] { kv =>
    valueList.map(kv.get) match {
      case ns :: Some(err) :: Some(errcode) :: st :: at :: Nil =>
        AssociationUnsupported(ns.getOrElse(namespaceOpenId11), err, errcode, st, at)
      case _ => sys.error("Invalid 'unsupported association response' "+ kv)
    }
  }
}
