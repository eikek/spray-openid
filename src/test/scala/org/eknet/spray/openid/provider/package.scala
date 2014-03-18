package org.eknet.spray.openid

import spray.httpx.marshalling.Marshaller
import spray.http.FormData
import spray.http.MediaTypes._
import org.eknet.spray.openid.model._

package object provider {

  implicit val CheckIdReqMarshaller =
    Marshaller.delegate[CheckIdRequest, FormData](`application/x-www-form-urlencoded`)(req => {
      FormData(req.toMap)
    })

}
