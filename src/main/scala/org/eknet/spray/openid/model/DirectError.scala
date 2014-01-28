package org.eknet.spray.openid.model

import spray.http.HttpEntity

case class DirectError(ns: String = namespaceOpenId2,
                       error: String,
                       contact: Option[String] = None,
                       reference: Option[String] = None) extends DirectResponse

object DirectError {
  val valueList = List("ns", "error", "contact", "reference")

  implicit val directErrorUnmarshaller = directResponseUnmarshaller[DirectError] { kv =>
    valueList.map(kv.get) match {
      case ns :: Some(error) :: contact :: ref :: Nil =>
        DirectError(ns.getOrElse(namespaceOpenId11), error, contact, ref)
      case _ => sys.error("Invalid error response")
    }
  }
}
