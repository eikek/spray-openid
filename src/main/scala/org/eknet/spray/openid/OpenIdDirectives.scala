package org.eknet.spray.openid

import spray.routing._
import scala.concurrent.ExecutionContext

trait OpenIdDirectives extends Directives with CheckIdDirectives with AssertionDirectives {

  def verifyId(auth: OpenIdAuth): Directive1[String] = auth.directive.map(_.claimedId)
}
