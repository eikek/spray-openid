package org.eknet.spray.openid.provider

import akka.actor.{ActorRef, Props, Actor, ActorLogging}
import akka.util.Timeout
import org.eknet.spray.openid.model.AssociationRequest.AssocType
import org.eknet.spray.openid.model.AssociationRequest.SessionType
import org.eknet.spray.openid.model._

object AssociationActor {
  def apply() = Props(classOf[AssociationActor])

  case class RemoveAssoc(handle: String)
  case class GetAssoc(handle: String)
  case class CreateAssoc(at: AssocType, st: SessionType, priv: Boolean)
  case class AssocHandle(handle: String, assoc: Association) {
    def toResponse = {
      val mac = assoc.cryptedMac.getOrElse(assoc.macKey.toArray).toBase64
      AssociationResponse(namespaceOpenId2, handle, assoc.stype.name, assoc.atype.name,
        assoc.expires.toSeconds.toInt, mac, assoc.keypair.map(_.getPublic.getY.toByteArray.toBase64))
    }
  }

  case class SignValues(handle: Option[String], values: Map[String, String])
  case class ValueSignature(handle: String, signed: List[String], sig: String)
  private case class SignValuesRef(req: SignValues, client: ActorRef, handle: AssocHandle)
}
class AssociationActor extends Actor with ActorLogging {
  import AssociationActor._
  import concurrent.duration._
  import context.dispatcher
  import akka.pattern.ask
  import akka.pattern.pipe
  private val timeout = 4.minutes

  private var publicMap = Map.empty[String, Association]
  private var privateMap = Map.empty[String, Association]

  override def receive = {
    case CreateAssoc(at, st, priv) =>
      val macKey = at.hmac.generateKey.get
      val assoc = if (st != SessionType.none) {
        val (p, g) = (BigInt(st.modulus.decodeBase64), BigInt(st.gen.decodeBase64))
        val keyPair = Crypt.DH.generateKeyPair(Crypt.DH.parameterSpec(p, g))
        Association(at, st, timeout, macKey.getEncoded.toVector, Some(keyPair.get))
      } else {
        Association(at, st, timeout, macKey.getEncoded.toVector, None)
      }
      val handle = Crypt.randomString
      if (priv) {
        privateMap = privateMap.updated(handle, assoc)
      } else {
        publicMap = publicMap.updated(handle, assoc)
      }
      context.system.scheduler.scheduleOnce(timeout, self, RemoveAssoc(handle))
      sender ! AssocHandle(handle, assoc)

    case r@AssociationRequest(st) =>
      self forward CreateAssoc(r.assocType, st, priv = false)

    case RemoveAssoc(handle) =>
      publicMap = publicMap - handle

    case GetAssoc(handle) =>
      sender ! publicMap.get(handle).map(a => AssocHandle(handle, a))

    case req: CheckAuthenticationRequest =>
      privateMap.get(req.assocHandle) match {
        case Some(assoc) =>
          val verifyer = Verification.verifySignature(assoc)
          val valid = verifyer(req.pos) match {
            case Right(p) => true
            case _ => false
          }
          sender ! CheckAuthenticationResponse(namespaceOpenId2, valid = valid,
            if (assoc.isValid) None else Some(req.assocHandle))
        case _ =>
          sender ! CheckAuthenticationResponse(namespaceOpenId2, valid = false, None)
      }

    case req @ SignValues(handle, _) =>
      implicit val to = Timeout(20.seconds)
      val client = sender
      handle.flatMap(publicMap.get) match {
        case Some(assoc) =>
          self ! SignValuesRef(req, client, AssocHandle(handle.get, assoc))
        case _ =>
          val f = (self ? CreateAssoc(AssocType.sha256, SessionType.none, priv = true))
            .mapTo[AssocHandle].map(h => SignValuesRef(req, client, h))
          f pipeTo self
      }

    case SignValuesRef(SignValues(_, values), client, AssocHandle(handle, assoc)) =>
      val sigdata = values + ("openid.assoc_handle" -> handle)
      val keys = sigdata.keys.toList
      val data = toKeyValueString(keys.map(k => k.substring(7) -> sigdata(k))).getBytes(io.Codec.UTF8.charSet)
      val sig = Crypt.sign(assoc.plainMacKey, data).get.toBase64
      client ! ValueSignature(handle, keys.map(k => k.substring(7)), sig)
  }
}
