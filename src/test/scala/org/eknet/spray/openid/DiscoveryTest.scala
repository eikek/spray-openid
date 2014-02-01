package org.eknet.spray.openid

import org.scalatest.{FunSuite, FlatSpec}
import org.scalatest.matchers.ShouldMatchers
import org.eknet.spray.openid.model.Discovery

class DiscoveryTest extends FunSuite with ShouldMatchers {

  test("Discovery HTML") {
    val in = io.Source.fromURL(getClass.getResource("/htmldiscovery.html"))
    val dis = Discovery.htmlDiscovery("test", in)
    assert(dis.providerUri === "https://www.myopenid.com/server")
    assert(dis.localId === None)
  }

  test("Discovery xrds") {
    val in = io.Source.fromURL(getClass.getResource("/xrds-google.xml"))
    val dis = Discovery.xrdsDiscovery("test", in)
    assert(dis.providerUri === "https://www.google.com/accounts/o8/ud")
    assert(dis.localId === None)
  }
}
