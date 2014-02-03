package org.eknet.spray.openid.model

import org.eknet.spray.openid.model.AssociationRequest._
import spray.http.FormData

case class AssociationRequest(sessionType: SessionType, assocType: AssocType = AssocType.sha256) extends DirectRequest {
  val mode = "associate"
}

object AssociationRequest {
  import spray.httpx.marshalling._

  case class AssocType(name: String, hmac: Crypt.Hmac)
  object AssocType {
    val sha1 = AssocType("HMAC-SHA1", Crypt.HmacSha1)
    val sha256 = AssocType("HMAC-SHA256", Crypt.HmacSha256)
    def fromString(s: String) = if (s.toUpperCase == sha1.name) sha1 else sha256
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
    private def encode(i: BigInt): String = i.toByteArray.toBase64

    def sha1(pubKey: String, modulus: String, gen: String): SessionType =
      TypeImpl("DH-SHA1", modulus, gen, pubKey, Crypt.Digest.sha1)

    def sha1(pubKey: BigInt, modulus: BigInt, gen: BigInt): SessionType =
      TypeImpl("DH-SHA1", encode(modulus), encode(gen), encode(pubKey), Crypt.Digest.sha1)

    def sha256(pubKey: String, modulus: String, gen: String): SessionType =
      TypeImpl("DH-SHA256", modulus, gen, pubKey, Crypt.Digest.sha256)

    def sha256(pubKey: BigInt, modulus: BigInt, gen: BigInt): SessionType =
      TypeImpl("DH-SHA256", encode(modulus), encode(gen), encode(pubKey), Crypt.Digest.sha256)

    val none: SessionType = TypeImpl("no-encryption", "", "", "", _ => sys.error("Not supported"))

    def apply(name: String, pubKey: String, modulus: String, gen: String): SessionType = {
      val h = if (name endsWith "SHA256") Crypt.Digest.sha256 else Crypt.Digest.sha1
      TypeImpl(name, modulus, gen, pubKey, h)
    }
  }

  implicit val AssociateReqMarshaller =
    Marshaller.delegate[AssociationRequest, FormData](`form-urlencoded`)(req => {
      FormData(filterNonEmpty(Map(
        "openid.ns" -> req.ns,
        "openid.mode" -> req.mode,
        "openid.assoc_type" -> req.assocType.name,
        "openid.session_type" -> req.sessionType.name,
        "openid.dh_gen" -> req.sessionType.gen,
        "openid.dh_modulus" -> req.sessionType.modulus,
        "openid.dh_consumer_public" -> req.sessionType.pubkey
      )))
    })

  private val fieldList = List("openid.ns", "openid.mode", "openid.assoc_type", "openid.session_type",
    "openid.dh_gen", "openid.dh_modulus", "openid.dh_consumer_public")

  implicit val AssociateReqUnmarshaller =
    directReqUnmarshaller[AssociationRequest] { fields =>
      fieldList.map(fields.get) match {
        case ns :: Some("associate") :: Some(at) :: Some(st) :: dgen :: dmod :: dpub :: Nil =>
          val session = dpub match {
            case Some(pkey) => SessionType(st, pkey, dmod.getOrElse(Crypt.DH.defaultModulusBase64), dgen.getOrElse(Crypt.DH.defaultGBase64))
            case _ => SessionType.none
          }
          AssociationRequest(session, AssocType.fromString(at))
        case _ => sys.error("Invalid association request: "+ fields)
      }
    }
}
