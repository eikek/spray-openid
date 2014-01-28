package org.eknet.spray.openid.model

import spray.http.{FormData, HttpEntity}

case class IndirectError(ns: String = namespaceOpenId2,
                         mode: String = "error",
                         error: String,
                         contact: Option[String] = None,
                         reference: Option[String] = None)


object IndirectError {

  val valueList = List("openid.ns", "openid.mode", "openid.error", "openid.contact", "openid.reference")

  implicit val IndirectErrorUnmarshaller =
    indirectReqUnmarshaller[IndirectError] { fd =>
      valueList.map(fd.get) match {
        case ns :: Some("error") :: Some(error) :: contact :: ref :: Nil =>
          IndirectError(ns.getOrElse(namespaceOpenId11), "error", error, contact, ref)
        case _ => sys.error("invalid indirect error response")
      }
    }
}
