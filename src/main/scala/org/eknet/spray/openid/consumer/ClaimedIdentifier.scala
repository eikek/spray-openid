package org.eknet.spray.openid.consumer

import java.net.{HttpURLConnection, URL}
import scala.concurrent.{ExecutionContext, Future}

object ClaimedIdentifier {

  /**
   * Normalizes the given identifier to a valid OpenId identifier as described in
   * [http://openid.net/specs/openid-authentication-2_0.html#normalization>]
   *
   * @param id
   * @return
   */
  def normalize(id: String)(implicit ec: ExecutionContext): Future[String] = {
    def isRedirect(code: Int) = code > 300 && code < 399
    //todo: check for infinite redirection
    @scala.annotation.tailrec
    def followRedirects(url: String): String = {
      val conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      conn.setInstanceFollowRedirects(false)
      if (isRedirect(conn.getResponseCode)) {
        val nextUrl = Option(conn.getHeaderField("Location")).filter(_.trim.nonEmpty) match {
          case Some(n) => n
          case _ => sys.error("Unable to follow url " + url)
        }
        conn.disconnect()
        followRedirects(nextUrl)
      } else {
        conn.disconnect()
        url
      }
    }
    val urlString = (stripXri andThen isXri andThen toUrl)(id)
    Future(followRedirects(urlString))
  }

  private val stripXri = (id: String) => if (id startsWith "xri://") id.substring(6) else id

  private val isXri = (id: String) => {
    val xriChars = Set('@', '=', '+', '$', '!', '(')
    if (id.isEmpty) throw new IllegalArgumentException("Empty id string")
    else if (xriChars contains id.charAt(0)) throw new UnsupportedOperationException("XRI uris are currently not supported")
    else id
  }

  private val toUrl = (id: String) => {
    val withScheme = if (id startsWith "http") id else "http://" + id
    withScheme.indexOf('#') match {
      case i if i > 0 => withScheme.substring(0, i)
      case _ => withScheme
    }
  }
}
