package org.eknet.spray.openid

import scala.xml.Node
import akka.actor.ActorSystem
import akka.util.Timeout
import spray.routing.SimpleRoutingApp
import spray.httpx.marshalling.Marshaller
import spray.http.MediaTypes._
import org.eknet.spray.openid.model.SimpleRegistration
import org.eknet.spray.openid.consumer._
import SimpleRegistration._

object RelyingParty extends SimpleRoutingApp with OpenIdDirectives with App {

  implicit val system = ActorSystem("openid-relyingparty")
  implicit val timeout = Timeout(4000)
  implicit val marshaller = Marshaller.delegate[Node, String](`text/html`)(_.toString())
  import system.dispatcher

  //actor doing verification of responses
  val verify = system.actorOf(VerifyActor(), name = "openid-verify")

  //additionally request these fields (simple registration extension)
  val fields = Field.fullname :: Field.email.asRequired :: Field.language :: Nil

  startServer("localhost", 8889) {
    pathEndOrSingleSlash {
      complete {
        <html>
          <head><title>Login Test</title></head>
          <body>
            <h1>Test Login</h1>
            <form method="post" action="verify">
              OpenID Identifier:
              <input type="text" name="openid_identifier"></input>
              <input type="submit" value="Sign In"></input>
            </form>
            <a href="/verify?openid_identifier=https://encrypted.google.com/accounts/o8/id">Sign in with Google</a>
          </body>
        </html>
      }
    } ~
    path("verify") {
      verifyId(OpenIdAuth(verify) ++ SimpleRegistration(fields)) { id =>
        complete {
          <html>
            <head><title>Login Result</title></head>
            <body>
              <h1>Login Successful</h1>
              <dl>
                <dt>Claimed Id:</dt><dd>{ id.claimedId }</dd>
                <dt>Fullname:</dt><dd>{  Field.fullname(id) }</dd>
                <dt>Email:</dt><dd>{ Field.email(id) }</dd>
                <dt>Language:</dt><dd>{ Field.language(id) }</dd>
              </dl>
            </body>
          </html>
        }
      } ~
      complete("Login failed.")
    }
  }
}
