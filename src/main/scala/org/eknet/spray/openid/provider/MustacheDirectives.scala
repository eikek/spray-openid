package org.eknet.spray.openid.provider

import spray.http._
import spray.routing._
import MediaTypes._
import org.eknet.spray.openid.model.CheckIdRequest
import org.eknet.spray.openid.provider.MustacheContext.KeyedData

trait MustacheDirectives extends Directives with MustacheContext.MoreConverter {

  def renderTemplate(mediaType: MediaType, template: Mustache.Template)(ctx: Mustache.Context) = {
    respondWithMediaType(mediaType) {
      complete(template(ctx))
    }
  }

  def renderHtmlTemplate(template: Mustache.Template)(ctx: Mustache.Context) = {
    respondWithMediaType(`text/html`) {
      complete(template(ctx))
    }
  }

  def requestContext(req: CheckIdRequest) = {
    KeyedData("params").putPairList(req.toMap)
      .andThen(KeyedData("realm").put(req.realm))
      .andThen(KeyedData("identity").put(req.identity))
      .andThen(KeyedData("claimedId").put(req.claimedId))
      .andThen(KeyedData("returnTo").put(req.returnTo))
  }
}