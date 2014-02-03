package org.eknet.spray.openid.model

/**
 * Extension requests "piggy back" on [[CheckIdRequest]]s and its responses on
 * [[PositiveAssertion]]s. Thus extension need to `ammend` [[CheckIdRequest]]s
 * and verify [[PositiveAssertion]]s on the relying party side. The provider side
 * needs to know which fields to include in the assertion response and additionally
 * execute some logic to the end user.
 *
 */
trait Extension {

  def namespace: String

  def verify(pos: PositiveAssertion): Either[String, PositiveAssertion]

  final def verifyFieldsInSignature(pos: PositiveAssertion, fields: Iterable[String]): Either[String, PositiveAssertion] = {
    val fieldSet = fields.toSet
    val intersect = pos.signed.toSet.intersect(fieldSet)
    if (intersect == fieldSet) Right(pos)
    else Left(s"The following fields are missing in the signature: ${fieldSet.diff(intersect)}")
  }

  def responseFields: String => Boolean
}

object Extension {
  type Ammend = CheckIdRequest => CheckIdRequest
}