package org.eknet.spray.openid.model

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class CryptTest extends FunSuite with ShouldMatchers {

  test("verify signature") {
    val key = Crypt.HmacSha1.generateKey.get
    val data = "abcdefg".getBytes
    val signed = Crypt.sign(key, data).get
    Crypt.verifySig(key)(signed, data).get should be (true)
    val badkey = Crypt.HmacSha1.generateKey.get
    Crypt.verifySig(badkey)(signed, data).get should be (false)
  }
}
