package org.eknet.spray.openid.model

trait DirectRequest {
  val ns = "http://specs.openid.net/auth/2.0"
  def mode: String
}
