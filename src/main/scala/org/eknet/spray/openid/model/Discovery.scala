package org.eknet.spray.openid.model

import java.net.{HttpURLConnection, URL}
import scala.xml._

object Discovery {
  private def isOpenIdHtmlLink(s: String) = s.contains("<link") && s.contains("rel=\"openid2.")

  private def xmlAttr(el: xml.Node, key: String): Option[String] =
    el.attribute(key).map(_.toList).getOrElse(Nil) match {
      case value :: Nil => Some(value.text)
      case _ => None
    }

  private[openid] def htmlDiscovery(givenId: String, document: io.Source): HtmlBased = {
    val openid2 = document.getLines().takeWhile(! _.contains("</head>")).withFilter(isOpenIdHtmlLink).map(XML.loadString)
    val result = openid2.foldLeft(HtmlBased(givenId, "", givenId)) { (dis, el) =>
      val next = for {
        rel <- xmlAttr(el, "rel")
        href <- xmlAttr(el, "href")
      } yield {
        if (rel == "openid2.provider") dis.copy(providerUri = href)
        else if (rel == "openid2.local_id") dis.copy(localId = href)
        else dis
      }
      next getOrElse dis
    }
    result.ensuring(_.providerUri.nonEmpty, "Html discovery failed. No provider url found")
  }

  private[openid] def xrdsDiscovery(givenId: String, document: io.BufferedSource): ServiceList = {
    def toService(node: Node): Option[ServiceElement] = {
      val p = xmlAttr(node, "priority").map(sp => if (sp == "null") Int.MaxValue else sp.toInt).getOrElse(10)
      val types = (node \ "Type" map (_.text)).map(_.trim).filter(_.nonEmpty).toList
      val uri = (node \ "URI" map (_.text) find(_.trim.nonEmpty)).getOrElse(sys.error("xrds discovery error: No provider url found")).trim
      val localId = node \ "LocalID" map (_.text) find (_.trim.nonEmpty)
      //op element has precedence
      if (types.exists(_ == OpElement.typeId)) {
        Some(OpElement(uri, types, p))
      }
      else if (types.exists(_ == ClaimedIdElement.typeId)) {
        Some(ClaimedIdElement(uri, localId, types, p))
      }
      else None
    }

    val xmlroot = XML.load(document.bufferedReader())
    val services = xmlroot.\\("Service").map(toService).collect({ case Some(x) => x }).toList.sortBy(_.priority)
    ServiceList(givenId, services.ensuring(_.nonEmpty, "Discovery error: No Service elements found"))
  }

  def discover(id: String): Discovered = {
    val url = new URL(id)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    if (conn.getResponseCode == 200) {
      val document = scala.io.Source.fromInputStream(conn.getInputStream)
      if (conn.getContentType.toLowerCase startsWith "text/html") {
        htmlDiscovery(id, document)
      } else {
        xrdsDiscovery(id, document)
      }
    }
    else sys.error(s"Got error response from '$id'")
  }

  sealed trait Discovered {
    val providerUri: String
    val claimedId: String
    val localId: String
  }

  sealed trait ServiceElement {
    def id: String
    def providerUri: String
    def priority: Int
    def types: List[String]
  }
  case class OpElement(providerUri: String, types: List[String], priority: Int) extends ServiceElement {
    val id = OpElement.typeId
  }
  object OpElement {
    val typeId = "http://specs.openid.net/auth/2.0/server"
  }
  case class ClaimedIdElement(providerUri: String, localId: Option[String], types: List[String], priority: Int) extends ServiceElement {
    val id = ClaimedIdElement.typeId
  }
  object ClaimedIdElement {
    val typeId = "http://specs.openid.net/auth/2.0/signon"
  }

  case class ServiceList(givenId: String, services: List[ServiceElement]) extends Discovered {
    require(services.nonEmpty, "The service list must not be empty")
    val element = services.find(_.id == OpElement.typeId).getOrElse(services.head)

    val claimedId = element match {
      case e: OpElement => identifierSelect
      case _ => givenId
    }
    val localId = element match {
      case e: OpElement => identifierSelect
      case ClaimedIdElement(_, lid, _, _) => lid.getOrElse(claimedId)
    }
    val providerUri = element.providerUri
  }
  case class HtmlBased(claimedId: String, providerUri: String, localId: String) extends Discovered

}
