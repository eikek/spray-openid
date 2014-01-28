package org.eknet.spray.openid

import org.eknet.spray.openid.model.PositiveAssertion
import scala.concurrent.duration.FiniteDuration
import spray.http.{Uri, DateTime}
import org.eknet.spray.openid.model.PositiveAssertion.ResponseNonce
import org.parboiled.common.Base64
import javax.crypto.spec.SecretKeySpec
import scala.io.Codec
import org.eknet.spray.openid.VerifyActor.Assoc
import scala.util.{Failure, Success}

object Verification {

  type Verifyer = PositiveAssertion => Either[String, PositiveAssertion]

  private def error(msg: String) = Left(msg)

  def verifyAll(uri: Uri, set: Set[ResponseNonce], timeout: FiniteDuration, assoc: Option[Assoc]): Verifyer = {
    val all = List(nonceTimeout(timeout), nonceOnetime(set), returnToRequest(uri)) ::: assoc.map(verifySignature).toList
    pos => {
      val el: Either[String, PositiveAssertion] = Right(pos)
      all.foldLeft(el) { (res, next) => res.right.flatMap(next) }
    }
  }

  def nonceTimeout(timeout: FiniteDuration): Verifyer = pos => {
    if (DateTime.now > (pos.nonce.time + timeout.toMillis)) error(s"Nonce '${pos.nonce}' is too old")
    else Right(pos)
  }

  def nonceOnetime(set: Set[ResponseNonce]): Verifyer = pos => {
    if (set contains pos.nonce) error(s"Nonce '${pos.nonce}' alread used!")
    else Right(pos)
  }

  def returnToRequest(req: Uri): Verifyer = pos => {
    val rto = Uri(pos.returnTo)
    val valid = (rto.withQuery("") == req.withQuery("")) &&
      rto.query.toMap.forall { case (key, value) => req.query.get(key) == Some(value) }
    if (valid) Right(pos) else error(s"ReturnTo url does not match request!")
  }

  def verifySignature(assoc: Assoc): Verifyer = pos => {
    val minimum = Set("op_endpoint", "return_to", "response_nonce", "assoc_handle", "claimed_id", "identity")
    if (!assoc.isValid) {
      error("Association response is not valid anymore.")
    }
    else if (!minimum.subsetOf(pos.signed.toSet)) {
      error("Missing fields in signature: "+ minimum.diff(pos.signed.toSet))
    }
    else {
      val mac = assoc.serverPubkey match {
        case Some(pb) => decryptMac(pb, assoc)
        case _ => Base64.rfc2045().decode(assoc.resp.macKey)
      }
      val mackey = new SecretKeySpec(mac, assoc.assocHmac.name)
      val theirsig = Base64.rfc2045().decode(pos.sig)
      Crypt.verifySig(mackey)(signedData(pos), theirsig) match {
        case Success(true) => Right(pos)
        case Success(false) => error(s"Signatures do not match.")
        case Failure(ex) => error(s"Error verifying signatures! ${ex.getMessage}")
      }
    }
  }

  private def decryptMac(pb: BigInt, assoc: VerifyActor.Assoc): Array[Byte] = {
    val zz = Crypt.DH.sharedSecret(pb, assoc.keypair.getPrivate.getX, assoc.modulus)
    val hzz = assoc.sessionType.hash(zz.toByteArray)
    val dmac = Base64.rfc2045().decode(assoc.resp.macKey)
    if (dmac.length != hzz.length) sys.error("Invalid length of hzz or mackey")
    hzz.zip(dmac).map {
      case (h, k) => (h ^ k).toByte
    }
  }

  private def signedData(pos: PositiveAssertion): Array[Byte] = {
    val data = pos.signed.map(key => s"$key:${pos.fields.get("openid."+key).getOrElse("")}\n").mkString("")
    data.getBytes(Codec.UTF8.charSet)
  }
}
