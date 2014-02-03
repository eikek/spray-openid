package org.eknet.spray.openid.consumer

import spray.routing._
import org.eknet.spray.openid.model._
import shapeless.{HNil, ::}
import akka.actor.ActorRef
import scala.concurrent.ExecutionContext
import akka.util.Timeout

trait AssertionDirectives extends Directives {

  def extractNegativeAssertion = entity(as[NegativeAssertion])

  def extractPositiveAssertion = entity(as[PositiveAssertion])

  def extractIndirectError = entity(as[IndirectError])

  private def openIdModeProvided: Directive1[Boolean] = anyParam("openid.mode".?).flatMap {
    case Some(_) => provide(true)
    case _ => provide(false)
  }

  def isAssertionRequest: Directive0 = openIdModeProvided.flatMap { flag =>
    if (flag) pass else reject()
  }

  def nonAssertionRequest: Directive0 = openIdModeProvided.flatMap { flag =>
    if (flag) reject() else pass
  }

  def verifyAssertion(verify: ActorRef)(implicit ec: ExecutionContext, to: Timeout): Directive1[PositiveAssertion] = new Directive1[PositiveAssertion] {
    def happly(f: (::[PositiveAssertion, HNil]) => Route) = {
      import akka.pattern.ask
      isAssertionRequest {
        extractIndirectError { err =>
          reject(ValidationRejection("Error response from OP: "+ err))
        } ~
        extractNegativeAssertion { neg =>
          reject(ValidationRejection("Negative assertion"))
        } ~
        extractPositiveAssertion { pos =>
          extract(_.request.uri) { uri =>
            val rq = VerifyActor.VerifyAssertion(uri, pos)
            onSuccess((verify ? rq).mapTo[VerifyActor.VerifyResult]) { res =>
              if (res.result) f(pos :: HNil) else reject(ValidationRejection("OpenId Assertion verification failed"))
            }
          }
        }
      }
    }
  }
}
