package org.eknet.spray.openid.provider

import spray.routing._
import org.eknet.spray.openid.model.CheckIdRequest
import org.eknet.spray.openid.model
import spray.http.Uri

trait ProviderHooks {
  type Account

  def accountIdentity: AccountIdentity[Account]

  /**
   * Authenticates the request and extracts the local account
   * object.
   *
   * @return
   */
  def authenticate(): Directive1[Account]

  /**
   * Renders the "login page", that allows the user to enter credentials.
   *
   * @param req
   * @return
   */
  def renderLoginPage(req: CheckIdRequest, endpoint: Uri): Route

  /**
   * Renders the "confirmation page", that allows the user after successful
   * authentication to revisit the authentication and optionally edit additional
   * values that are transfered by extensions means.
   *
   * The `req` is the request that is currently processed. It must be retained
   * to correctly match the next request. Thus the form displayed by this page
   * should resend all values of `req`.
   *
   * @param req
   * @param a
   * @return
   */
  def renderConfirmationPage(req: CheckIdRequest, a: Account, endpoint: Uri): Route

  /**
   * Determines (passes) whether the request denotes a user cancellation. A
   * negative assertion is sent to the relying party in this case.
   *
   * @return
   */
  def isUserCancel: Directive0

  /**
   * Determines (passes) whether the request denotes a user submission. A
   * positive assertion is sent to the relying party in this case.
   * @return
   */
  def isUserSubmit: Directive0

  /**
   * Determine for the given account, whether to display the "confirmation page"
   * (pass) or not (reject). If rejected, the assertion is sent immediately to
   * the relying party.
   * @param a
   * @return
   */
  def skipConfirmation(a: Account): Directive0
}

/** Creates the identity url from the local account/account name and vice versa. */
trait AccountIdentity[A] {
  def toIdentityUrl(a: A): String
  def fromIdentityUrl(url: String): String
}
object AccountIdentity {

  def urlSuffix(baseUrl: String) = new AccountIdentity[String] {
    def fromIdentityUrl(url: String) = url.substring(baseUrl.length)
    def toIdentityUrl(name: String) =
      if (baseUrl.endsWith("/")) baseUrl + name else baseUrl +"/"+ name
  }
}

object ProviderHooks {
  val defaultLoginTemplate = Mustache(getClass.getResourceAsStream("login-template.mustache"))
  val defaultConfirmTemplate = Mustache(getClass.getResourceAsStream("continue-template.mustache"))

  def apply[A](usernameIdentity: AccountIdentity[A])(auth: Directive1[A]) =
    new SimpleHooks(defaultLoginTemplate, defaultConfirmTemplate, usernameIdentity)(auth)

  def apply[A](loginTempl: Mustache.Template, confirmTempl: Mustache.Template, usernameIdentity: AccountIdentity[A])(auth: Directive1[A]) =
    new SimpleHooks(loginTempl, confirmTempl, usernameIdentity)(auth)

  class SimpleHooks[A](loginTempl: Mustache.Template, confirmTempl: Mustache.Template, usernameIdentity: AccountIdentity[A])(auth: Directive1[A])
    extends ProviderHooks with MustacheDirectives with ProviderDirectives with Directives {
    import MustacheContext._

    private val renderLogin = renderHtmlTemplate(loginTempl)_
    private val renderConfirm = renderHtmlTemplate(confirmTempl)_
    private val extract: String => Option[String] = url =>
      if (url == model.identifierSelect) None else Some(usernameIdentity.fromIdentityUrl(url))

    type Account = A
    def skipConfirmation(a: A) = reject()

    def isUserSubmit = formField("spray-openid.submitType").flatMap {
      case "continue" => pass
      case _ => reject()
    }
    def isUserCancel = formField("spray-openid.submitType").flatMap {
      case "cancel" => pass
      case _ => reject()
    }

    def renderConfirmationPage(req: CheckIdRequest, a: A, endpoint: Uri) = {
      val ctx = requestContext(req).andThen(KeyedData("endpointUrl").put(endpoint.toString()))
      renderConfirm(ctx(empty))
    }

    def renderLoginPage(req: CheckIdRequest, endpoint: Uri) = {
      formField("spray-openid.submitType".?) { st =>
        val context = requestContext(req)
          .andThen(KeyedData("endpointUrl").put(endpoint.toString()))
          .andThen(KeyedData("loginFailed").put(st.exists(_ == "signin")))
          .andThen(KeyedData("username").put(extract(req.identity)))
        renderLogin(context(empty))
      }
    }

    def authenticate() = auth

    def accountIdentity = usernameIdentity
  }
}