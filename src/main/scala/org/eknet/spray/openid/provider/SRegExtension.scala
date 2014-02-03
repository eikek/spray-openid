package org.eknet.spray.openid.provider

import org.eknet.spray.openid.model.{SimpleRegistration, CheckIdRequest}
import org.eknet.spray.openid.provider.MustacheContext.MoreConverter
import java.util.Locale

object SRegExtension extends MoreConverter {
  import MustacheContext._
  import SimpleRegistration._
  import org.eknet.spray.openid.model._

  implicit object FieldValueConverter extends MapConverter[(Field, Option[String])] {
    def convert(data: (Field, Option[String])) = {
      val (obj, value) = data
      val map = obj.name match {
        case Field.language.name =>
          LocaleField(obj.fullname, obj.name, value, Locale.getDefault).toMap
        case Field.timezone.name =>
          TimezoneField(obj.fullname, obj.name, value, Locale.getDefault).toMap
        case Field.ns.name =>
          TextField(obj.fullname, obj.name, Some(SimpleRegistration.namespace), hidden = true).toMap
        case _ =>
          TextField(obj.fullname, obj.name, value).toMap
      }
      map.updated("required", obj.required)
    }
  }

  implicit val fieldConverter = new MapConverter[Field] {
    override def convert(obj: Field) = FieldValueConverter.convert((obj, None))
  }

  case class SRegData(policyUrl: Option[String], fields: List[Field], values: Map[String, String])
  object SRegData {
    implicit object SRegDataConverter extends MapConverter[SRegData] {
      override def convert(obj: SRegData) = {
        val ns = if (obj.fields.nonEmpty) List(Field.ns) else Nil
        val fieldvals = obj.fields.map(f => f -> obj.values.get(f.name))
        val ctx = KeyedData("policy_url").put(obj.policyUrl)
          .andThen(KeyedData("attributesExist").put(obj.fields.nonEmpty))
          .andThen(KeyedData("attributes").put(fieldvals))
          .andThen(KeyedData("hiddenAttributes").put(ns))
        ctx(empty)
      }
    }
  }

  def createSRegData(req: CheckIdRequest, values: Map[String, String]): SRegData = {
    def extractFields(kind: String) =
      for {
        fvals <- req.adds.get(s"openid.sreg.$kind").toList
        fname <- splitString(fvals, ',')
        field <- Field.all.find(_.name == fname)
      } yield field.copy(required = kind == "required")

    val fields = extractFields("optional") ::: extractFields("required")
    val purl = req.adds.get("openid.sreg.policy_url")
    SRegData(purl, fields, values)
  }

  def formFields(req: CheckIdRequest, values: Map[String, String] = Map.empty) =
    Data.append(createSRegData(req, values))
}
