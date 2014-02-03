package org.eknet.spray.openid.consumer

import spray.http.{Uri, StatusCodes}
import org.eknet.spray.openid.model.{Extension, Discovery, CheckIdRequest}
import spray.routing.Directives
import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import akka.actor.{ActorRefFactory, ActorRef}
import org.eknet.spray.openid.model.Discovery.{HtmlElement, ServiceList}
import org.eknet.spray.openid.consumer.VerifyActor.AssociateResult

trait CheckIdDirectives extends Directives {

  def redirectCheckIdRequest(verify: ActorRef, suppliedId: String, returnTo: String,
                             realm: Option[String], immediate: Boolean, ext: Seq[Extension.Ammend] = Nil)
                            (implicit refFactory: ActorRefFactory, ec: ExecutionContext, to: Timeout) =
    onSuccess(setupCheckIdRequest(verify, suppliedId, returnTo, realm, immediate, ext)) { uri =>
      redirect(uri, StatusCodes.Found)
    }

  def setupCheckIdRequest(verify: ActorRef, suppliedId: String, returnTo: String,
                          realm: Option[String], immediate: Boolean, ext: Seq[Extension.Ammend] = Nil)
                         (implicit refFactory: ActorRefFactory, ec: ExecutionContext, to: Timeout) = {
    import ClaimedIdentifier.normalize
    import Discovery.{discover, Discovered}
    import akka.pattern.ask

    def createRequestUri(ar: AssociateResult, discovered: Discovery.Discovered, normalizedId: String): Uri = {
      val (claimedId, localId) = discovered match {
        case s: ServiceList => (s.claimedId, s.localId)
        case h: HtmlElement => (normalizedId, h.localId.getOrElse(normalizedId))
      }
      val orgReq = CheckIdRequest(claimedId, returnTo, localId, ar.handle, realm, immediate)
      val req = ext.foldLeft(orgReq) { (r, e) => e(r) }
      req.appendToQuery(Uri(discovered.providerUri))
    }

    def associate(discovered: Discovered) = {
      (verify ? VerifyActor.Associate(Uri(discovered.providerUri))).mapTo[VerifyActor.AssociateResult]
    }

    for {
      normed <- normalize(suppliedId)
      discovered <- discover(normed)
      assoc <- associate(discovered)
    } yield createRequestUri(assoc, discovered, normed)
  }
}
