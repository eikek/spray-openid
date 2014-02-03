package org.eknet.spray.openid.provider

import org.eknet.spray.openid.model.{SimpleRegistration, CheckIdRequest}
import org.eknet.spray.openid.provider.MustacheContext.MoreConverter
import java.util.Locale

object SRegExtension extends MoreConverter {
  import MustacheContext._
  import SimpleRegistration._
  import org.eknet.spray.openid.model._

  implicit object FieldConverter extends MapConverter[Field] {
    override def convert(obj: Field) = {
      val map = obj.name match {
        case Field.language.name =>
          LocaleField(obj.fullname, obj.name, None, Locale.getDefault).toMap
        case Field.timezone.name =>
          TimezoneField(obj.fullname, obj.name, None, Locale.getDefault).toMap
        case Field.ns.name =>
          TextField(obj.fullname, obj.name, Some(SimpleRegistration.namespace), hidden = true).toMap
        case _ =>
          TextField(obj.fullname, obj.name, None).toMap
      }
      map.updated("required", obj.required)
    }
  }

  def formFields(req: CheckIdRequest) = {
    def extractFields(kind: String) =
      for {
        fvals <- req.adds.get(s"openid.sreg.$kind").toList
        fname <- splitString(fvals, ',')
        field <- Field.all.find(_.name == fname)
      } yield field.copy(required = kind == "required")

    val fields = extractFields("optional") ::: extractFields("required")
    val ns = if (fields.nonEmpty) List(Field.ns) else Nil
    val purl = req.adds.get("openid.sreg.policy_url")
    val ctx = KeyedData("policy_url").put(purl)
      .andThen(KeyedData("attributesExist").put(fields.nonEmpty))
      .andThen(KeyedData("attributes").put(fields))
      .andThen(KeyedData("hiddenAttributes").put(ns))

    Data.appendRaw(ctx(empty))
  }
}
