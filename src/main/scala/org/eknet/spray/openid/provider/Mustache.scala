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

import scala.util.parsing.combinator.RegexParsers
import scala.util.Try
import java.io.InputStream
import scala.io.Source

object Mustache {
  import Template._
  import TemplateParser._
  type Context = Map[String, Any]

  def tryApply(str: String): Try[Template] = Try(makeTemplate(parse(str)))
  def tryApply(in: InputStream): Try[Template] = Try {
    val s = Source.fromInputStream(in).getLines().mkString("\n")
    tryApply(s).get
  }

  def apply(str: String): Template = tryApply(str).get
  def apply(in: InputStream): Template = tryApply(in).get

  private def makeTemplate(tokens: List[Token]): Template = {
    def loop(ts: List[Token], template: Template = Template.empty): Template = {
      ts match {
        case Nil => template
        case t::tx => t match {
          case StringToken(s) => loop(tx, template ++ Text(s))
          case LookupToken(n) => loop(tx, template ++ Lookup(n))
          case BlockStart(n, i) =>
            val (h, t) = tx.span(!_.isBlockEnd(n))
            if (t == Nil) throw new Exception(s"No end block for '$n'")
            loop(t, template ++ Block(n, i, loop(h)))
          case BlockEnd(n) => loop(tx, template)
        }
      }
    }
    loop(tokens)
  }

  trait Template extends (Context => String) {
    def ++(t: Template): Template = Composite(Vector(this, t))
  }

  object Template {
    val empty = new Template {
      def apply(ctx: Context): String = ""
      override def ++(t: Template) = t
      override def toString() = "empty()"
    }
    case class Composite(ts: Vector[Template]) extends Template {
      def apply(ctx: Context): String =
        ts.foldLeft(StringBuilder.newBuilder) { (s, t) => s append t(ctx) }.toString()
      override def ++(t: Template): Template = Composite(ts :+ t)
    }
    case class Text(s: String) extends Template {
      def apply(v1: Mustache.Context): String = s
    }
    case class Lookup(name: String) extends Template {
      def apply(ctx: Context): String = ctx.get(name) match {
        case Some(a) => a.toString
        case _ => ""
      }
    }
    case class Block(name: String, inverse: Boolean, inner: Template) extends Template {
      private val emptyValues = Set("", false, None, 0, null, Map(), List())
      private def createContext(context: Context, name: String): Seq[Context] = {
        context(name) match {
          case map: Map[_, _] => Seq(map.asInstanceOf[Map[String, Any]])
          case seq: Iterable[_] =>
            seq.headOption match {
              case Some(h : Map[_, _]) => seq.asInstanceOf[Seq[Context]]
              case Some(h) => seq.map(e => Map("." -> e)).toList
              case _ => Seq.empty
            }
          case obj if !emptyValues.contains(obj) => Seq(context)
          case _ => Seq.empty
        }
      }
      def apply(context: Context): String = {
        if (inverse) {
          context.get(name) match {
            case Some(x) if !emptyValues.contains(x) => ""
            case _ => inner(context)
          }
        } else {
          val seq = Try(createContext(context, name)).getOrElse(Seq.empty[Context])
          seq.foldLeft("") { (s, ctx) => s + inner(ctx) }
        }
      }
    }
  }

  object TemplateParser {
    sealed trait Token {
      def isBlockEnd(name: String): Boolean = false
    }
    case class BlockStart(name: String, inverse: Boolean) extends Token
    case class BlockEnd(name: String) extends Token {
      override def isBlockEnd(name: String): Boolean = this.name == name
    }
    case class LookupToken(name: String) extends Token
    case class StringToken(str: String) extends Token

    def parse(templateStr: String): List[Token] = Parse.parse(templateStr)

    private object Parse extends RegexParsers {
      override def skipWhitespace = false
      val deliStart = "{{"
      val deliEnd = "}}"

      val sectionName = "[\\w\\s\\.\\-_]+".r
      val arbitraryText = rep1(not(deliStart) ~> ".|\r|\n".r) ^^ { s => StringToken(s.mkString) }
      val blockStart = deliStart ~ ("#" | "^") ~ sectionName ~ deliEnd ^^ {
        case d1 ~ t ~ section ~ d2 => BlockStart(section, inverse = t == "^")
      }
      val blockEnd = deliStart ~ "/" ~ sectionName ~ deliEnd ^^ {
        case d1 ~ x ~ section ~ d2 => BlockEnd(section)
      }
      val contextLookup = deliStart ~ sectionName ~ deliEnd ^^ {
        case d1 ~ name ~ d2 => LookupToken(name)
      }
      val token = contextLookup | blockStart | blockEnd | arbitraryText
      def tokens = rep(token)

      def parse(template: String): List[Token] = parseAll(tokens, template) match {
        case Success(r, _) => r
        case Failure(msg, next) => throw new Exception(msg+": "+next.source + "@"+next.pos.line+":"+next.pos.column)
        case Error(msg, next) =>  throw new Exception(msg+": "+next.source + "@"+next.pos.line+":"+next.pos.column)
      }
    }
  }
}