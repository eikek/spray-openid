package org.eknet.spray.openid

import spray.http._
import scala.util.{Success, Try}
import org.parboiled.common.Base64
import spray.httpx.marshalling.Marshaller
import MediaTypes._

package object model {
  import spray.httpx.unmarshalling._

  val identifierSelect = "http://specs.openid.net/auth/2.0/identifier_select"
  val namespaceOpenId11 = "http://openid.net/signon/1.1"
  val namespaceOpenId2 = "http://specs.openid.net/auth/2.0"

  val `application/xrds+xml` = {
    val mt = MediaType.custom(mainType = "application", subType = "xrds+xml")
    MediaTypes.register(mt)
    mt
  }

  def toKeyValueResponse(s: String): Map[String, String] = {
    s.split('\n').map(_.trim).withFilter(_.nonEmpty).map { line =>
      line.split(":", 2).toList match {
        case key::value::Nil => key -> value
        case p => sys.error("Invalid key-value response: "+p)
      }
    }.toMap
  }

  def toKeyValueString(data: Seq[(String, String)]): String = {
    data.map { case (k, v) => s"$k:$v\n" }.mkString("")
  }

  def directResponseUnmarshaller[A](f: Map[String, String] => A): Unmarshaller[A] = {
    Unmarshaller(`text/plain`) {
      case HttpEntity.NonEmpty(ct, data) =>
        val kv = filterNonEmpty(toKeyValueResponse(data.asString))
        f(kv)
    }
  }

  def directResponseMarshaller[A](f: A => Map[String, String]): Marshaller[A] = {
    Marshaller.of(`text/plain`) { (a, ct, ctx) =>
      ctx.marshalTo(toKeyValueString(f(a).toSeq))
    }
  }

  def directReqUnmarshaller[A](f: Map[String, String] => A): Unmarshaller[A] = {
    Unmarshaller.delegate[FormData, A](`application/x-www-form-urlencoded`) { fdata =>
      f(fdata.fields.toMap)
    }
  }

  def indirectReqUnmarshaller[A](f: Map[String, String] => A): FromRequestUnmarshaller[A] = {
    def protect(map: Map[String, String]) = Try(f(map)) match {
      case Success(v) => Right(v)
      case _ => Left(MalformedContent("unable to unmarshall entity: "+ map))
    }
    new FromRequestUnmarshaller[A] {
      def apply(req: HttpRequest) = {
        if (req.uri.query.nonEmpty) {
          protect(req.uri.query.toMap)
        } else {
          req.entity.as[FormData].right.map(_.fields.toMap) match {
            case Right(fd) => protect(fd)
            case Left(ex) => Left(ex)
          }
        }
      }
    }
  }

  def filterNonEmpty(map: Map[String, String]) = map.filterNot(_._2.isEmpty)

  def splitString(s: String, c: Char) = s.split(c).map(_.trim).filter(_.nonEmpty).toList

  implicit class ToBase64(bytes: Array[Byte]) {
    def toBase64 = Base64.rfc2045().encodeToString(bytes, false)
  }
  implicit class FromBase64(str: String) {
    def decodeBase64 = Base64.rfc2045().decode(str)
  }
  implicit class UriAdds(uri: Uri) {
    def appendToQuery(map: Map[String, String]): Uri = uri.withQuery(uri.query.toMap ++ map)
  }
  def uriMatchesRealm(returnto: Uri, realm: Uri): Boolean = {
    lazy val scheme = returnto.scheme == realm.scheme
    lazy val port = returnto.effectivePort == realm.effectivePort
    lazy val path = returnto.path.startsWith(realm.path)
    lazy val nofragm = realm.fragment.isEmpty
    lazy val domain =
      if (realm.authority.host.address.contains("*.")) {
        val realmHost = realm.authority.host.address.substring(1).toLowerCase
        val uriHost = "."+returnto.authority.host.address.toLowerCase
        uriHost.endsWith(realmHost)
      } else {
        realm.authority == returnto.authority
      }
    scheme && port && path && nofragm && domain
  }
}
