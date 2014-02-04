package org.eknet.spray.openid.provider

import spray.routing._
import spray.http._
import org.eknet.spray.openid.model.{SimpleRegistration, PositiveAssertion, NegativeAssertion, CheckIdRequest}
import akka.actor.ActorRef
import org.eknet.spray.openid.provider.AssociationActor.{ValueSignature, SignValues}
import org.eknet.spray.openid.model.PositiveAssertion.ResponseNonce
import scala.concurrent.ExecutionContext
import akka.util.Timeout

trait ProviderDirectives extends Directives {

  def isImmediateRequest: Directive0 = anyParam("openid.mode").flatMap {
    case "checkid_immediate" => pass
    case _ => reject()
  }

  def withValidReturnTo(req: CheckIdRequest)(f: Uri => Route): Route = {
    req.returnTo match {
      case Some(str) if req.returnToMatchesRealm => f(Uri(str))
      case _ =>
        complete {
          HttpResponse(status = StatusCodes.BadRequest,
            entity = "No returnTo url given or does not match realm.")
        }
    }
  }

  def redirectNegativeAssertion(req: CheckIdRequest, ass: NegativeAssertion): Route = {
    withValidReturnTo(req) { uri =>
      redirect(ass.appendToQuery(uri), StatusCodes.Found)
    }
  }

  def redirectPositiveAssertion(req: CheckIdRequest, identity: String, endpoint: Uri, assocRef: ActorRef)
                               (implicit ec: ExecutionContext, to: Timeout): Route = {
    import org.eknet.spray.openid.model._
    import akka.pattern.ask
    val values = (req.toMap ++ Map(
      "openid.ns" -> namespaceOpenId2,
      "openid.identity" -> identity,
      "openid.response_nonce" -> ResponseNonce.next.asString,
      "openid.op_endpoint" -> endpoint.toString,
      "openid.mode" -> "id_res"
    )).filterKeys(responseFieldFilter)

    val f = (assocRef ? SignValues(req.assocHandle, values)).mapTo[ValueSignature]
    onSuccess(f) { sig =>
      val pos = values ++ Map(
        "openid.sig" -> sig.sig,
        "openid.signed" -> sig.signed.mkString(","),
        "openid.assoc_handle" -> sig.handle
      )

      withValidReturnTo(req) { uri =>
        redirect(uri.appendToQuery(pos), StatusCodes.Found)
      }
    }
  }

  private val responseFieldFilter: String => Boolean = field =>
    PositiveAssertion.fieldList.contains(field) || SimpleRegistration.responseFields(field)
}
