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
package io.gatling.http.action

import io.gatling.core.action.SessionHook
import io.gatling.core.protocol.ProtocolComponentsRegistry
import io.gatling.http.protocol.HttpProtocol
import io.gatling.core.session.{ Expression, Session }
import io.gatling.core.structure.ScenarioContext
import io.gatling.core.validation.{ FailureWrapper, SuccessWrapper }
import io.gatling.http.cookie.CookieSupport.storeCookie

import akka.actor.{ ActorSystem, ActorRef }
import org.asynchttpclient.cookie.Cookie
import org.asynchttpclient.uri.Uri

case class CookieDSL(name: Expression[String], value: Expression[String],
                     domain: Option[Expression[String]] = None,
                     path: Option[Expression[String]] = None,
                     maxAge: Option[Long] = None) {

  def withDomain(domain: Expression[String]) = copy(domain = Some(domain))
  def withPath(path: Expression[String]) = copy(path = Some(path))
  def withMaxAge(maxAge: Int) = copy(maxAge = Some(maxAge))
}

object AddCookieBuilder {

  val NoBaseUrlFailure = "Neither cookie domain nor baseURL".failure
  val RootSuccess = "/".success
  val DefaultPath: Expression[String] = _ => RootSuccess
}

class AddCookieBuilder(name: Expression[String], value: Expression[String], domain: Option[Expression[String]], path: Option[Expression[String]], maxAge: Long) extends HttpActionBuilder {

  import AddCookieBuilder._

  private def defaultDomain(httpProtocol: HttpProtocol) = {
    val baseUrlHost = httpProtocol.baseURL.map(url => Uri.create(url).getHost)
    (session: Session) => baseUrlHost match {
      case Some(host) => host.success
      case _          => NoBaseUrlFailure
    }
  }

  def build(system: ActorSystem, ctx: ScenarioContext, protocolComponentsRegistry: ProtocolComponentsRegistry, next: ActorRef): ActorRef = {

    val hc = httpComponents(protocolComponentsRegistry)
    val resolvedDomain = domain.getOrElse(defaultDomain(hc.httpProtocol))
    val resolvedPath = path.getOrElse(DefaultPath)

    val expression: Expression[Session] = session => for {
      name <- name(session)
      value <- value(session)
      domain <- resolvedDomain(session)
      path <- resolvedPath(session)
      cookie = new Cookie(name, value, false, domain, path, maxAge, false, false)
    } yield storeCookie(session, domain, path, cookie)

    system.actorOf(SessionHook.props(expression, ctx.coreComponents.statsEngine, next, interruptable = true), actorName("addCookie"))
  }
}
