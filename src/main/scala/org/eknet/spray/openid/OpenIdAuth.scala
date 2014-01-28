package org.eknet.spray.openid

import scala.concurrent.ExecutionContext
import org.eknet.spray.openid.model.PositiveAssertion
import spray.routing._
import shapeless.{HNil, ::}
import akka.util.Timeout
import akka.actor.ActorRef

case class OpenIdAuth(verify: ActorRef,
                      suppliedId: Option[String] = None,
                      realm: Option[String] = None,
                      immediate: Boolean = false)(implicit ec: ExecutionContext, to: Timeout)
  extends AssertionDirectives with CheckIdDirectives {

  private def redirectToOp(id: String, returnTo: String) =
    redirectCheckIdRequest(verify, id, returnTo, realm.orElse(Some(returnTo)), immediate)

  val directive = new Directive1[PositiveAssertion] {
    def happly(f: (::[PositiveAssertion, HNil]) => Route) = {
      verifyAssertion(verify)(ec, to) { pos =>
        f(pos :: HNil)
      } ~
      nonAssertionRequest {
        extract(_.request.uri.toString()) { returnTo =>
          suppliedId match {
            case Some(id) => redirectToOp(id, returnTo)
            case _ => anyParam("openid_identifier") { id =>
              redirectToOp(id, returnTo)
            }
          }
        }
      }
    }
  }
}
