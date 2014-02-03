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

import Mustache.Context
import java.util.{TimeZone, Locale}
import spray.http.Uri

/**
 * Helps with populating [[scala.collection.Map]]s for use with [[Mustache]]
 */
object MustacheContext {

  val empty = Map.empty[String, Any]

  object Data {
    def append[T](value: T)(implicit conv: MapConverter[T]): Context => Context =
      _ ++ unwrapOption(value.toMap)
    def appendRaw(value: Context): Context => Context = _ ++ unwrapOption(value)
  }

  class KeyedData(name: String) {
    def put[T](value: T)(implicit conv: ValueConverter[T]): Context => Context =
      _ ++ unwrapOption(Map(name -> value.toValue))
    def putIf[T](cond: => Boolean, value: T)(implicit conv: ValueConverter[T]): Context => Context =
      if (cond) put(value)(conv) else identity

    def putRaw(value: Context): Context => Context = _.updated(name, unwrapOption(value))
    def putRaw(value: List[Any]): Context => Context = _.updated(name, value)
    def putPairList(value: Context): Context => Context = putRaw(pairList(value))
  }
  object KeyedData {
    def apply(name: String) = new KeyedData(name)
  }

  private def pairList(data: Context): List[Context] = {
    (data map { case (k, v) => Map("name" -> k, "value" -> v) }).toList
  }

  private def unwrapOption(m: Map[String, Any]): Map[String, Any] = {
    m.filterNot(_._2 == None).mapValues {
      case Some(x) => x
      case m: Map[_, _] => unwrapOption(m.asInstanceOf[Map[String, Any]])
      case l: List[_] => l.headOption match {
        case Some(m: Map[_, _]) => l.map(m => unwrapOption(m.asInstanceOf[Map[String, Any]]))
        case _ => l
      }
      case x => x
    }
  }
  trait ValueConverter[T] {
    def convert(obj: T): Any
  }
  trait MapConverter[T] extends ValueConverter[T] {
    def convert(obj: T): Map[String, Any]
  }

  implicit class ToContextValue[T](obj: T) {
    def toValue(implicit conv: ValueConverter[T]) = conv.convert(obj)
    def toMap(implicit conv: MapConverter[T]) = conv.convert(obj)
  }

  trait BasicConverter {
    final class PassConv[T] extends ValueConverter[T] {
      def convert(obj: T) = obj
    }
    implicit val stringConv = new PassConv[String]
    implicit val intConv = new PassConv[Int]
    implicit val longConv = new PassConv[Long]
    implicit val boolConv = new PassConv[Boolean]

    implicit def optionValueConv[T](implicit conv: ValueConverter[T]) = new ValueConverter[Option[T]] {
      def convert(obj: Option[T]) = obj match {
        case Some(x) => x.toValue
        case _ => None
      }
    }
    implicit def listConv[T](implicit conv: ValueConverter[T]) = new ValueConverter[List[T]] {
      def convert(obj: List[T]) = conv match {
        case mc: MapConverter[T] => obj.map(v => unwrapOption(v.toMap(mc)))
        case _ => obj.map(_.toValue)
      }
    }
    implicit def mapConv[T](implicit conv: ValueConverter[T]) = new ValueConverter[Map[String, T]] {
      def convert(obj: Map[String, T]) = unwrapOption(obj.mapValues(_.toValue))
    }
  }
  object BasicConverter extends BasicConverter

  trait MoreConverter extends BasicConverter {
    case class TextField(name: String, label: String, value: Option[String], hidden: Boolean = false)
    implicit object TextFieldConv extends MapConverter[TextField] {
      def convert(obj: TextField) = {
        Map(
          "name" -> obj.name,
          "label" -> obj.label,
          "value" -> obj.value,
          "type" -> (if (obj.hidden) "hidden" else "text")
        )
      }
    }

    case class SelectField(name: String, label: String, value: Option[String], values: List[(String, String)])
    implicit object SelectFieldConv extends MapConverter[SelectField] {
      def convert(obj: SelectField) = {
        val options = for ((id, name) <- obj.values)
        yield Map("id" -> id, "name" -> name, "selected" -> (obj.value == Some(id)))
        TextField(obj.name, obj.label, obj.value).toValue match {
          case m: Map[_, _] => m.asInstanceOf[Map[String, Any]] ++ Map("values" -> options, "select" -> true)
          case _ => sys.error("Expected map object")
        }
      }
    }

    case class LocaleField(name: String, label: String, value: Option[String], locale: Locale)
    implicit object LocaleFieldConv extends MapConverter[LocaleField] {
      def convert(obj: LocaleField) = {
        val allLocales =
          for (l <- Locale.getAvailableLocales)
          yield l.toLanguageTag -> l.getDisplayName(obj.locale)

        SelectField(obj.name, obj.label, obj.value, ("", "None") :: allLocales.toList).toMap
      }
    }

    case class TimezoneField(name: String, label: String, value: Option[String], locale: Locale)
    implicit object TimezoneFieldConv extends MapConverter[TimezoneField] {
      def convert(obj: TimezoneField) = {
        val tzs =
          for (id <- TimeZone.getAvailableIDs) yield {
            val tz = TimeZone.getTimeZone(id)
            val name = tz.getDisplayName(obj.locale) +" ("+id+")"
            id -> name
          }
        SelectField(obj.name, obj.label, obj.value, ("", "None") :: tzs.toList).toMap
      }
    }

    implicit object UriConv extends ValueConverter[Uri] {
      def convert(obj: Uri) = obj.toString()
    }
  }
  object MoreConverter extends MoreConverter
}
