package org.eknet.spray.openid

import spray.http.{Uri, StatusCodes}
import org.eknet.spray.openid.model.{Discovery, CheckIdRequest}
import spray.routing.Directives
import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import akka.actor.ActorRef

trait CheckIdDirectives extends Directives {

  def redirectCheckIdRequest(verify: ActorRef, suppliedId: String, returnTo: String,
                             realm: Option[String], immediate: Boolean, adds: Map[String, String] = Map.empty)
                            (implicit ec: ExecutionContext, to: Timeout) =
    onSuccess(setupCheckIdRequest(verify, suppliedId, returnTo, realm, immediate, adds)) { uri =>
      redirect(uri, StatusCodes.Found)
    }

  def setupCheckIdRequest(verify: ActorRef, suppliedId: String, returnTo: String,
                          realm: Option[String], immediate: Boolean, adds: Map[String, String] = Map.empty)
                         (implicit ec: ExecutionContext, to: Timeout) = {
    import ClaimedIdentifier.normalize
    import Discovery.{discover, Discovered}
    import akka.pattern.ask

    def associate(discovered: Discovered): Future[(VerifyActor.AssociateResult, Discovered)] = {
      val f = (verify ? VerifyActor.Associate(Uri(discovered.providerUri))).mapTo[VerifyActor.AssociateResult]
      f.map(r => (r, discovered))
    }

    normalize(suppliedId).map(discover).flatMap(associate).map { case (ar, discovered) =>
      val req = CheckIdRequest(discovered.claimedId, returnTo, discovered.localId, ar.handle, realm, adds, immediate)
      req.appendToQuery(Uri(discovered.providerUri))
    }
  }

}
