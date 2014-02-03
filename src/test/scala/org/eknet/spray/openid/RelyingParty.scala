package org.eknet.spray.openid

import akka.actor.ActorSystem
import akka.util.Timeout
import spray.routing.SimpleRoutingApp
import spray.http.{MediaTypes, ContentType, HttpEntity}
import org.eknet.spray.openid.model.SimpleRegistration
import org.eknet.spray.openid.consumer._

object RelyingParty extends SimpleRoutingApp with OpenIdDirectives with App {

  implicit val system = ActorSystem("openid-relyingparty")
  import system.dispatcher

  implicit val timeout = Timeout(4000)
  val verify = system.actorOf(VerifyActor(), name = "openid-verify")

  val html = ContentType(MediaTypes.`text/html`)
  val loginPage =
    """
      |<html>
      |<head><title>Login test</title></head>
      |<body>
      |  <h1> Test Login </h1>
      |  <form method="post" action="verify">
      |    Open Identifier:
      |    <input type="text" name="openid_identifier" value="">
      |    <input type="submit" value="Sign in">
      |  </form>
      |  <a href="/verify?openid_identifier=https://encrypted.google.com/accounts/o8/id">Sign in with Google</a>
      |</body>
      |</html>
    """.stripMargin


  import SimpleRegistration._
  val fields = Seq(Field.fullname, Field.email.asRequired, Field.language)

  startServer("localhost", 8889) {
    path("") {
      complete(HttpEntity(html, loginPage))
    } ~
    path("verify") {
      verifyId(OpenIdAuth(verify) ++ SimpleRegistration(fields) ++ policyUrl("http://google.com")) { id =>
        val fullname = Field.fullname(id)
        val email = Field.email(id)
        val lang = Field.language(id)
        complete(s"Login Successful: ${id.claimedId}\nFullname: $fullname\nEmail: $email\nLanguage: $lang")
      } ~
      complete("Login failed.")
    }
  }
}
