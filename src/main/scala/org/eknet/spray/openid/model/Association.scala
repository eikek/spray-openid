package org.eknet.spray.openid.model

import org.eknet.spray.openid.model.AssociationRequest.{SessionType, AssocType}
import scala.concurrent.duration.FiniteDuration
import javax.crypto.spec.SecretKeySpec

case class Association(atype: AssocType,
                       stype: SessionType,
                       expires: FiniteDuration,
                       macKey: Vector[Byte],
                       keypair: Option[Crypt.DH.DHKeyPair]) {

  private val validUntil = System.currentTimeMillis() + expires.toMillis
  def isValid = System.currentTimeMillis() < validUntil

  lazy val cryptedMac = keypair.map { kp =>
    require(stype != SessionType.none, "Session type does not support encryption")
    val (ya, p, g) = (BigInt(stype.pubkey.decodeBase64), BigInt(stype.modulus.decodeBase64), BigInt(stype.gen.decodeBase64))
    val pubkey = Crypt.DH.publicKey(ya, p, g)
    val zz = Crypt.DH.sharedSecret(pubkey, kp.getPrivate, p)
    val hzz = stype.hash(zz.toByteArray)
    if (hzz.length != macKey.length) sys.error("Invalid length of macKey or hzz")
    hzz.zip(macKey).map { case (h, k) => (h ^ k).toByte }
  }

  lazy val cryptedMacKey = cryptedMac.map(cm => new SecretKeySpec(cm, atype.hmac.name))
  lazy val plainMacKey = new SecretKeySpec(macKey.toArray, atype.hmac.name)
}
