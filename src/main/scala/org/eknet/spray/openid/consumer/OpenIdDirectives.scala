package org.eknet.spray.openid.consumer

import spray.routing._
import scala.concurrent.ExecutionContext

trait OpenIdDirectives extends Directives with CheckIdDirectives with AssertionDirectives {

  def verifyId(auth: OpenIdAuth): Directive1[Assertion] =
    auth.directive.map(p => Assertion(p.claimedId, p.fields))

}
object OpenIdDirectives extends OpenIdDirectives

case class Assertion(claimedId: String, fields: Map[String, String])
