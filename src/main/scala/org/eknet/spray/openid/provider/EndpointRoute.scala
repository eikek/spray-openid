package org.eknet.spray.openid.provider

import spray.routing._
import org.eknet.spray.openid.model._
import scala.concurrent.ExecutionContext
import akka.util.Timeout
import org.eknet.spray.openid.provider.AssociationActor.AssocHandle
import org.eknet.spray.openid.model
import org.eknet.spray.openid.model.SimpleRegistration.Field

/**
 * Sketches the outline of the openid endpoint. You have to fill in the gaps
 * by providing the [[ProviderHooks]] impl.
 *
 * @param settings
 */
class EndpointRoute(settings: EndpointSettings) extends Directives with ProviderDirectives {
  import akka.pattern.ask

  def route(implicit ec: ExecutionContext, to: Timeout): Route = {
    entity(as[AssociationRequest]) { req =>
      complete {
        (settings.assocRef ? req).mapTo[AssocHandle].map(_.toResponse)
      }
    } ~
    entity(as[CheckAuthenticationRequest]) { req =>
      complete {
        (settings.assocRef ? req).mapTo[CheckAuthenticationResponse]
      }
    } ~
    entity(as[CheckIdRequest]) { req =>
      settings.hook.isUserCancel {
        redirectNegativeAssertion(req, NegativeAssertion.cancel())
      } ~
      settings.hook.authenticate() { id =>
        readyForAssertion(req, id) {
          redirectPositiveAssertion(req, settings.hook.accountIdentity.toIdentityUrl(id), settings.assocRef)
        } ~
        endpointUri { endpoint =>
          val identity = settings.hook.accountIdentity.toIdentityUrl(id)
          val request = if (req.claimedId == model.identifierSelect) {
            req.copy(claimedId = identity, identity = identity)
          } else {
            req.copy(identity = identity)
          }
          settings.hook.renderConfirmationPage(request, id, endpoint)
        }
      } ~
      endpointUri { endpoint =>
        if (req.isImmediate) redirectNegativeAssertion(req, NegativeAssertion.setupNeeded())
        else settings.hook.renderLoginPage(req, endpoint)
      }
    }
  }

  private def readyForAssertion(req: CheckIdRequest, id: settings.hook.Account): Directive0 =
    if (req.isImmediate) pass
    else settings.hook.isUserSubmit | (noExtension & settings.hook.skipConfirmation(id))

  private def noExtension: Directive0 = formField(Field.ns.fullname.?).flatMap {
    case Some(_) => reject()
    case _ => pass
  }
}
