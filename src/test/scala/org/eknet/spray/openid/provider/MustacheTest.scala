/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.eknet.spray.openid.provider

import org.scalatest.{Matchers, FunSuite}
import scala.util.Failure

class MustacheTest extends FunSuite with Matchers {
  import Mustache._
  import Mustache.TemplateParser._

  test("parse to token stream") {
    val templ = " Hi, I'm {{name}}, so far {{#list}} its {{item}} time {{/list}}."
    val tokens = TemplateParser.parse(templ)
    val expect = List(StringToken(" Hi, I'm "),
      LookupToken("name"), StringToken(", so far "),
      BlockStart("list", false), StringToken(" its "),
      LookupToken("item"), StringToken(" time "),
      BlockEnd("list"), StringToken(".")
    )
    tokens.size should be (expect.size)
    tokens.zip(expect).foreach { case (t, e) =>
      assert(t === e)
    }
  }

  test("makeTemplate") {
    val templ = " Hi, I'm {{name}}, so far{{#list}} its {{item}} time{{/list}}."
    val template = Mustache(templ)
    val context = Map("name" -> "Eike", "list" -> Seq(Map("item" -> "1st"), Map("item"->"2nd")))
    template(context) should be (" Hi, I'm Eike, so far its 1st time its 2nd time.")

    val template2 = Mustache("{{#musketeers}}\n* {{.}}\n{{/musketeers}}")
    val out2 = template2(Map("musketeers" -> List("Athos", "Aramis", "Porthos")))
    out2 should be ("\n* Athos\n\n* Aramis\n\n* Porthos\n")
  }

  test("erroneous templates") {
    val Failure(e) = Mustache.tryApply("{{#list}} {{.}} as")
  }

  test("provided templates") {
    Mustache(getClass.getResourceAsStream("/org/eknet/spray/openid/provider/login-template.mustache"))
    Mustache(getClass.getResourceAsStream("/org/eknet/spray/openid/provider/continue-template.mustache"))
  }

  test("inverse blocks") {
    val template = Mustache("Hallo, {{^name}}Unknown{{/name}}{{#name}}{{name}}{{/name}}.")
    val s1 = template(Map("test" -> "other"))
    s1 should be ("Hallo, Unknown.")
    val s2 = template(Map("name" -> "John"))
    s2 should be ("Hallo, John.")
    val s3 = template(Map("name" -> List()))
    s3 should be ("Hallo, Unknown.")
  }

  test("empty blocks") {
    val template = Mustache("Hello {{#flag}}Sir {{/flag}}Fauntleroy")
    val s1 = template(Map("flag" -> false))
    s1 should be ("Hello Fauntleroy")

    val s2 = template(Map("flag" -> true))
    s2 should be ("Hello Sir Fauntleroy")
  }
}
