package org.eknet.spray.openid.provider

import spray.http.Uri
import spray.routing._
import spray.http.Uri.Path
import shapeless.HNil

case class DiscoverySettings(endpointUrl: Uri, xrdsServer: Directive0, identityPath: Directive0) {
  def endpointMatcher: PathMatcher0 = {
    import PathMatchers._
    endpointUrl.path match {
      case Path.Empty => ""
      case Path.Slash(Path.Empty) => ""
      case p if p.startsWithSlash => separateOnSlashes(p.dropChars(1).toString())
      case p => separateOnSlashes(p.toString())
    }
  }
}

object DiscoverySettings {

  def forPathPrefix(baseUrl: Uri): DiscoverySettings = {
    import Directives._
    val ep = Uri("/openid/endpoint").resolvedAgainst(baseUrl.noSlashAtEnd)
    val xrdsserver = (baseUrl.noSlashAtEnd.path.toString + "/openid/auth").drop(1)
    val xrdsid: PathMatcher0 = (baseUrl.toRelative.path match {
      case Path.Slash(_) => Segment
      case Path.Empty => Segment
      case p => separateOnSlashes(p.toString()) / Segment
    }).hmap(_ => HNil)
    DiscoverySettings(ep, path(separateOnSlashes(xrdsserver)), path(xrdsid))
  }

  private implicit class UriAdds(uri: Uri) {
    def noSlashAtEnd: Uri = {
      if (uri.path.toString() == "/") uri.withPath(Path.Empty)
      else if (uri.path.reverse.startsWithSlash) uri.withPath(uri.path.reverse.dropChars(1).reverse)
      else uri
    }
  }
}
