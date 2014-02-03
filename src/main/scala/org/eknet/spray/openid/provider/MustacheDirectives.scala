package org.eknet.spray.openid.provider

import spray.http._
import spray.routing._
import org.eknet.spray.openid.model.CheckIdRequest
import org.eknet.spray.openid.provider.MustacheContext.KeyedData

trait MustacheDirectives extends Directives with MustacheContext.MoreConverter {
  import MustacheDirectives._

  def renderTemplate(ct: ContentType, template: Mustache.Template)(ctx: Mustache.Context) = {
    complete(HttpEntity(ct, template(ctx)))
  }

  def renderHtmlTemplate(template: Mustache.Template)(ctx: Mustache.Context) = {
    renderTemplate(contentTypeHtml, template)(ctx)
  }

  def requestContext(req: CheckIdRequest) = {
    KeyedData("params").putPairList(req.toMap)
      .andThen(KeyedData("realm").put(req.realm))
      .andThen(KeyedData("identity").put(req.identity))
      .andThen(KeyedData("claimedId").put(req.claimedId))
      .andThen(KeyedData("returnTo").put(req.returnTo))
  }
}

object MustacheDirectives extends MustacheDirectives {

  val contentTypeHtml = ContentType(MediaTypes.`text/html`)

}