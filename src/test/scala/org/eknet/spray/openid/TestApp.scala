package org.eknet.spray.openid

import spray.routing.SimpleRoutingApp
import akka.actor.ActorSystem
import spray.http.{MediaTypes, ContentType, HttpEntity}
import akka.util.Timeout

object TestApp extends SimpleRoutingApp with OpenIdDirectives with App {

  implicit val system = ActorSystem("testing-openid")
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


  startServer("localhost", 8889) {
    path("login") {
      complete(HttpEntity(html, loginPage))
    } ~
    path("verify") {
      verifyId(OpenIdAuth(verify)) { id =>
        complete("Login Successful: "+ id)
      } ~
      complete("Login failed.")
    }
  }
}
