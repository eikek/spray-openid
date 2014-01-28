package org.eknet.spray.openid

import java.net.{HttpURLConnection, URL}
import scala.xml._

object Discovery {
  private def isOpenIdHtmlLink(s: String) = s.contains("<link") && s.contains("rel=\"openid2.")

  private def xmlAttr(el: xml.Node, key: String): Option[String] =
    el.attribute(key).map(_.toList).getOrElse(Nil) match {
      case value :: Nil => Some(value.text)
      case _ => None
    }

  private[openid] def htmlDiscovery(document: io.Source): HtmlBased = {
    val openid2 = document.getLines().takeWhile(! _.contains("</head>")).withFilter(isOpenIdHtmlLink).map(XML.loadString)
    val result = openid2.foldLeft(HtmlBased("", "", None)) { (dis, el) =>
      val next = for {
        rel <- xmlAttr(el, "rel")
        href <- xmlAttr(el, "href")
      } yield {
        if (rel == "openid2.provider") dis.copy(providerUri = href)
        else if (rel == "openid2.local_id") dis.copy(localId = Some(href))
        else dis
      }
      next getOrElse dis
    }
    result.ensuring(_.providerUri.nonEmpty, "Html discovery failed. No provider url found")
  }

  private[openid] def xrdsDiscovery(document: io.BufferedSource): ServiceList = {
    def toService(node: Node) = {
      val p = xmlAttr(node, "priority").map(sp => if (sp == "null") Int.MaxValue else sp.toInt).getOrElse(10)
      val types = node \ "Type" map (_.text)
      val uri = node \ "URI" map (_.text) find(_.trim.nonEmpty)
      val localId = node \ "LocalID" map (_.text) find (_.trim.nonEmpty)
      Service(
        uri.getOrElse(sys.error("xrds discovery error: No provider url found")).trim,
        localId,
        types.map(_.trim).filter(_.nonEmpty).toList,
        p
      )
    }

    val xmlroot = XML.load(document.bufferedReader())
    val services = xmlroot.\\("Service").map(toService).toList.sortBy(_.priority)
    ServiceList("", services.ensuring(_.nonEmpty, "Discovery error: No Service elements found"))
  }

  def discover(id: String): Discovered = {
    val url = new URL(id)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    if (conn.getResponseCode == 200) {
      val document = scala.io.Source.fromInputStream(conn.getInputStream)
      if (conn.getContentType.toLowerCase startsWith "text/html") {
        htmlDiscovery(document).copy(claimedId = id)
      } else {
        xrdsDiscovery(document).copy(claimedId = id)
      }
    }
    else sys.error(s"Got error response from '$id'")
  }

  sealed trait Discovered {
    val providerUri: String
    val localId: Option[String]
    val claimedId: String
  }
  case class Service(providerUri: String, localId: Option[String], types: List[String], priority: Int)
  case class ServiceList(claimedId: String, services: List[Service]) extends Discovered {
    val providerUri = services(0).providerUri
    val localId = services(0).localId
  }
  case class HtmlBased(claimedId: String, providerUri: String, localId: Option[String]) extends Discovered

}
