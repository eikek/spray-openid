package org.eknet.spray.openid.model

import org.eknet.spray.openid.model.AssociationRequest._
import org.eknet.spray.openid.model.AssociationRequest.AssocType.AssocType
import spray.http.FormData
import org.eknet.spray.openid.Crypt
import org.parboiled.common.Base64

case class AssociationRequest(sessionType: SessionType, assocType: AssocType = AssocType.sha256) extends DirectRequest {
  val mode = "associate"
}

object AssociationRequest {
  import spray.httpx.marshalling._

  object AssocType extends Enumeration {
    type AssocType = Value
    val sha1 = Value("HMAC-SHA1")
    val sha256 = Value("HMAC-SHA256")
  }
  sealed trait SessionType {
    def name: String
    def modulus: String
    def gen: String
    def pubkey: String
    def hash(msg: Array[Byte]): Array[Byte]
  }
  object SessionType {
    private case class TypeImpl(name: String, modulus: String, gen: String, pubkey: String, h: Array[Byte] => Array[Byte]) extends SessionType {
      def hash(msg: Array[Byte]) = h(msg)
    }
    private val base64 = Base64.rfc2045()
    private def encode(i: BigInt): String = base64.encodeToString(i.toByteArray, false)

    def sha1(pubKey: String, modulus: String = Crypt.DH.defaultModulusBase64, gen: String = Crypt.DH.defaultGString): SessionType =
      TypeImpl("DH-SHA1", modulus, gen, pubKey, Crypt.Digest.sha1)

    def sha1(pubKey: BigInt, modulus: BigInt, gen: BigInt): SessionType =
      TypeImpl("DH-SHA1", encode(modulus), encode(gen), encode(pubKey), Crypt.Digest.sha1)

    def sha256(pubKey: String, modulus: String = Crypt.DH.defaultModulusBase64, gen: String = Crypt.DH.defaultGString): SessionType =
      TypeImpl("DH-SHA256", modulus, gen, pubKey, Crypt.Digest.sha256)

    def sha256(pubKey: BigInt, modulus: BigInt, gen: BigInt): SessionType =
      TypeImpl("DH-SHA256", encode(modulus), encode(gen), encode(pubKey), Crypt.Digest.sha256)

    val none: SessionType = TypeImpl("no-encryption", "", "", "", _ => sys.error("Not supported"))

    def apply(pubKey: String, modulus: String = Crypt.DH.defaultModulusBase64, gen: String = Crypt.DH.defaultGString): SessionType =
      sha256(pubKey, modulus, gen)
  }

  implicit val AssociateReqMarshaller =
    Marshaller.delegate[AssociationRequest, FormData](`form-urlencoded`)(req => {
      FormData(filterNonEmpty(Map(
        "openid.ns" -> req.ns,
        "openid.mode" -> req.mode,
        "openid.assoc_type" -> req.assocType.toString,
        "openid.session_type" -> req.sessionType.name,
        "openid.dh_gen" -> req.sessionType.gen,
        "openid.dh_modulus" -> req.sessionType.modulus,
        "openid.dh_consumer_public" -> req.sessionType.pubkey
      )))
    })
}
