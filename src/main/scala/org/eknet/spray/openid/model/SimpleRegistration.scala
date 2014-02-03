package org.eknet.spray.openid.model

import org.eknet.spray.openid.consumer.Assertion

object SimpleRegistration extends Extension {
  import Extension.Ammend

  val namespace = "http://openid.net/sreg/1.0"

  def apply(field: Field, fields: Field*): Ammend = apply(fields :+ field)

  def apply(fields: Seq[Field]): Ammend = req => {
    val (reqs, opts) = fields.partition(_.required)
    req supplement Map (
      Field.ns.fullname -> namespace,
      "openid.sreg.required" -> reqs.map(_.name).mkString(","),
      "openid.sreg.optional" -> opts.map(_.name).mkString(",")
    )
  }

  def policyUrl(url: String): Ammend =
    req => req.supplement(Map("openid.sreg.policy_url" -> url))

  def responseFields = allNames.contains

  def verify(pos: PositiveAssertion) = {
    val sregfields = pos.fields.keys.filter(responseFields).map(_.substring(7)).toSet
    verifyFieldsInSignature(pos, sregfields)
  }

  case class Field(name: String, required: Boolean = false) {
    lazy val fullname = "openid.sreg."+name
    def apply(pos: Assertion): Option[String] = pos.fields.get(fullname)
    def asOptional = copy(required = false)
    def asRequired = copy(required = true)
  }

  private val allNames = Field.all.map(k => k.fullname).toSet + Field.ns.fullname
  object Field {
    val ns = new Field("sreg") {
      override lazy val fullname = "openid.ns.sreg"
    }

    val nickname = Field("nickname")
    val email = Field("email")
    val fullname = Field("fullname")
    val dateOfBirth = Field("dob")
    val gender = Field("gender")
    val postcode = Field("postcode")
    val country = Field("country")
    val language = Field("language")
    val timezone = Field("timezone")
    val all = Seq(nickname, email, fullname, dateOfBirth, gender, postcode, country, language, timezone)
  }
}
