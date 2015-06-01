/**
 * Copyright 2011-2015 eBusiness Information, Groupe Excilys (www.ebusinessinformation.fr)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.gatling.http.cache

import scala.annotation.tailrec

import io.gatling.core.config.GatlingConfiguration
import io.gatling.core.util.cache.SessionCacheHandler
import io.gatling.core.session.{ SessionPrivateAttributes, Session }
import io.gatling.http.ahc.HttpTx

import org.asynchttpclient.{ Request, RequestBuilder }
import org.asynchttpclient.uri.Uri

object PermanentRedirectCache {
  val HttpPermanentRedirectCacheAttributeName = SessionPrivateAttributes.PrivateAttributePrefix + "http.cache.redirects"
}

trait PermanentRedirectCache {

  def configuration: GatlingConfiguration

  private val httpPermanentRedirectCacheHandler = new SessionCacheHandler[Uri, Uri](PermanentRedirectCache.HttpPermanentRedirectCacheAttributeName, configuration.http.perUserCacheMaxCapacity)

  def addRedirect(session: Session, from: Uri, to: Uri): Session =
    httpPermanentRedirectCacheHandler.addEntry(session, from, to)

  private def permanentRedirect(session: Session, uri: Uri): Option[(Uri, Int)] = {

      @tailrec def permanentRedirect1(from: Uri, redirectCount: Int): Option[(Uri, Int)] =

        httpPermanentRedirectCacheHandler.getEntry(session, from) match {
          case Some(toUri) => permanentRedirect1(toUri, redirectCount + 1)

          case None => redirectCount match {
            case 0 => None
            case _ => Some((from, redirectCount))
          }
        }

    permanentRedirect1(uri, 0)
  }

  private def redirectRequest(request: Request, toUri: Uri): Request = {
    val requestBuilder = new RequestBuilder(request)
    requestBuilder.setUri(toUri)
    requestBuilder.build()
  }

  def applyPermanentRedirect(origTx: HttpTx): HttpTx =
    if (origTx.request.config.httpComponents.httpProtocol.requestPart.cache)
      permanentRedirect(origTx.session, origTx.request.ahcRequest.getUri) match {
        case Some((targetUri, redirectCount)) =>

          val newAhcRequest = redirectRequest(origTx.request.ahcRequest, targetUri)

          origTx.copy(request = origTx.request.copy(
            ahcRequest = newAhcRequest),
            redirectCount = origTx.redirectCount + redirectCount)

        case None => origTx
      }
    else
      origTx
}
