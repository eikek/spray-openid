package org.eknet.spray.openid

import spray.http._
import scala.util.{Success, Try}

package object model {
  import spray.httpx.unmarshalling._

  val identifierSelect = "http://specs.openid.net/auth/2.0/identifier_select"
  val namespaceOpenId11 = "http://openid.net/signon/1.1"
  val namespaceOpenId2 = "http://specs.openid.net/auth/2.0"

  val `text/plain` = MediaTypes.`text/plain`
  val `form-urlencoded` = MediaTypes.`application/x-www-form-urlencoded`

  def toKeyValueResponse(s: String): Map[String, String] = {
    s.split('\n').map(_.trim).withFilter(_.nonEmpty).map { line =>
      line.split(":", 2).toList match {
        case key::value::Nil => key -> value
        case p => sys.error("Invalid key-value response: "+p)
      }
    }.toMap
  }

  def directResponseUnmarshaller[A](f: Map[String, String] => A): Unmarshaller[A] = {
    Unmarshaller(`text/plain`) {
      case HttpEntity.NonEmpty(ct, data) =>
        val kv = filterNonEmpty(toKeyValueResponse(data.asString))
        f(kv)
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
}
