package org.eknet.spray.openid

import org.eknet.spray.openid.model.PositiveAssertion

object SimpleRegistration {
  private val namespace = Map("openid.ns.sreg" -> "http://openid.net/sreg/1.0")

  def sreg(field: Field, fields: Field*): Map[String, String] = {
    sreg(fields :+ field)
  }

  def sreg(fields: Seq[Field]): Map[String, String] = {
    (fields.partition(_.required) match { case (rq, opt) =>
      Map(
        "openid.sreg.required" -> rq.map(_.name).mkString(","),
        "openid.sreg.optional" -> opt.map(_.name).mkString(",")
      )
    }) ++ namespace
  }

  def policyUrl(url: String) = Map("openid.sreg.policy_url" -> url)

  def verify(pos: PositiveAssertion): Either[String, PositiveAssertion] = {
    val sregs = pos.fields.filterKeys(_ startsWith "openid.sreg.").map(_._1.substring(7)).toSet
    val incl = pos.signed.toSet.intersect(sregs)
    if (incl == sregs) Right(pos)
    else Left("openid.sreg fields are not included in signature: "+ sregs.diff(incl))
  }

  case class Field(name: String, required: Boolean = false) {
    def apply(pos: Assertion): Option[String] = pos.fields.get(s"openid.sreg.$name")
    def optional = copy(required = false)
    def require = copy(required = true)
  }

  object Field {
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
