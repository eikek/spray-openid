package org.eknet.spray.openid.model

import java.net.{HttpURLConnection, URL}
import scala.xml._
import spray.http.{HttpResponse, HttpRequest, HttpEntity, ContentTypeRange}
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.ActorRefFactory

object Discovery {
  import spray.client.pipelining._

  def discover(idUrl: String)(implicit refFactory: ActorRefFactory, ec: ExecutionContext): Future[Discovered] = {
    implicit val unmarshaller = Discovered.responseUnmarshaller(idUrl)
    val pipeline: HttpRequest => Future[Discovered] = sendReceive ~> unmarshal[Discovered]
    pipeline(Get(idUrl))
  }
  

  sealed trait Discovered {
    def protocol: String
    def providerUri: String
  }

  object Discovered {
    import spray.httpx.unmarshalling._

    def responseUnmarshaller(claimedId: String) = new Unmarshaller[Discovered] {
      def apply(entity: HttpEntity) = {
        DiscoveredUnmarshaller(entity).right.map {
          case ServiceList("", elems) => ServiceList(claimedId, elems)
          case HtmlElement("", lid) => HtmlElement(claimedId, lid)
          case s => s
        }
      }
    }

    val DiscoveredUnmarshaller = new Unmarshaller[Discovered] {
      def apply(entity: HttpEntity) = {
        ServiceList.XrdsUnmarshaller(entity).left.flatMap(_ => HtmlElement.HtmlUnmarshaller(entity))
      }
    }
  }

  sealed trait ServiceElement {
    def protocol: String
    def providerUri: String
    def priority: Int
    def types: List[String]
    def toXml: xml.Node
  }

  case class OpElement(providerUri: String, types: List[String], priority: Int) extends ServiceElement {
    val protocol = OpElement.version
    def toXml = {
      <Service priority={priority.toString}>
        { types.map(t => <Type>{t}</Type>) }
        <URI>{ providerUri }</URI>
      </Service>
    }
  }
  object OpElement {
    val version = "http://specs.openid.net/auth/2.0/server"
  }

  case class ClaimedIdElement(providerUri: String, localId: Option[String], types: List[String], priority: Int) extends ServiceElement {
    val protocol = ClaimedIdElement.version
    def toXml =
      <Service priority={priority.toString}>
        { types.map(t => <Type>{t}</Type>) }
        <URI>{ providerUri }</URI>
        { localId.map(lid => <LocalID>{lid}</LocalID>) }
      </Service>
  }
  object ClaimedIdElement {
    val version = "http://specs.openid.net/auth/2.0/signon"
  }

  case class ServiceList(givenId: String, services: List[ServiceElement]) extends Discovered {
    require(services.nonEmpty, "The service list must not be empty")
    val element = services.find(_.protocol == OpElement.version).getOrElse(services.head)
    val protocol = element.protocol
    val claimedId = element match {
      case e: OpElement => identifierSelect
      case _ => givenId
    }
    val localId = element match {
      case e: OpElement => identifierSelect
      case ClaimedIdElement(_, lid, _, _) => lid.getOrElse(claimedId)
    }
    val providerUri = element.providerUri

    def toXml =
      <xrds:XRDS xmlns:xrds="xri://$xrds" xmlns="xri://$xrd*($v*2.0)">
        <XRD>
          { services.map(_.toXml) }
        </XRD>
      </xrds:XRDS>
  }

  object ServiceList {
    import spray.httpx.unmarshalling._
    import spray.httpx.marshalling._

    val XrdsUnmarshaller = Unmarshaller(`application/xrds+xml`) {
      case HttpEntity.NonEmpty(ct, data) =>
        val xmlroot = XML.loadString(data.asString)
        val services = xmlroot.\\("Service").map(toService).collect({ case Some(x) => x }).toList.sortBy(_.priority)
        ServiceList("", services.ensuring(_.nonEmpty, "Discovery error: No Service elements found"))
    }

    implicit val XrdsMarshaller = Marshaller.of[ServiceList](`application/xrds+xml`) { (xrds, ct, ctx) =>
      ctx.marshalTo(HttpEntity(ct, xrds.toXml.toString()))
    }

    private def toService(node: Node): Option[ServiceElement] = {
      def xmlAttr(el: xml.Node, key: String): Option[String] =
        el.attribute(key).map(_.toList).getOrElse(Nil) match {
          case value :: Nil => Some(value.text)
          case _ => None
        }
      val p = xmlAttr(node, "priority").map(sp => if (sp == "null") Int.MaxValue else sp.toInt).getOrElse(10)
      val types = (node \ "Type" map (_.text)).map(_.trim).filter(_.nonEmpty).toList
      val uri = (node \ "URI" map (_.text) find(_.trim.nonEmpty)).getOrElse(sys.error("xrds discovery error: No provider url found")).trim
      val localId = node \ "LocalID" map (_.text) find (_.trim.nonEmpty)
      //op element has precedence
      if (types.exists(_ == OpElement.version)) {
        Some(OpElement(uri, types, p))
      }
      else if (types.exists(_ == ClaimedIdElement.version)) {
        Some(ClaimedIdElement(uri, localId, types, p))
      }
      else None
    }

  }

  case class HtmlElement(providerUri: String, localId: Option[String]) extends Discovered {
    val protocol = ClaimedIdElement.version
  }

  object HtmlElement {
    import spray.httpx.unmarshalling._
    private val linkRegex = """(?s)<link.*?(href|rel)=(.*?)(href|rel)=(.*?)>""".r
    private val hrefRegex = """.*?href=("|')(.*?)("|').*""".r

    val HtmlUnmarshaller = Unmarshaller(ContentTypeRange(`text/html`)) {
      case HttpEntity.NonEmpty(ct, data) =>
        val html = data.asString
        val fi = html.indexOf("<head>")
        val ei = html.indexOf("</head>", fi)
        val links = for {
          m <- linkRegex.findAllMatchIn(html.substring(fi, ei))
          if m.matched contains "rel=\"openid2."
        } yield m.matched

        links.foldLeft(HtmlElement("", None)) { (dis, n) =>
          n match {
            case hrefRegex(_, href, _) =>
              if (n contains "openid2.provider") dis.copy(providerUri = href)
              else if (n contains "openid2.local_id") dis.copy(localId = Some(href))
              else dis
            case _ => dis
          }
        }
    }
  }
}
