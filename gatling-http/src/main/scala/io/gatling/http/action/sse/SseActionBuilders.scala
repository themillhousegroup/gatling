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
package io.gatling.http.action.sse

import io.gatling.core.config.GatlingConfiguration
import io.gatling.core.protocol.ProtocolComponentsRegistry

import akka.actor.{ ActorSystem, ActorRef }
import io.gatling.core.session.Expression
import io.gatling.core.structure.ScenarioContext
import io.gatling.http.action.HttpActionBuilder
import io.gatling.http.check.ws.WsCheckBuilder
import io.gatling.http.request.builder.sse.SseOpenRequestBuilder

class SseOpenActionBuilder(requestName: Expression[String],
                           sseName: String,
                           requestBuilder: SseOpenRequestBuilder,
                           checkBuilder: Option[WsCheckBuilder] = None)(implicit configuration: GatlingConfiguration) extends HttpActionBuilder {

  def check(checkBuilder: WsCheckBuilder) = new SseOpenActionBuilder(requestName, sseName, requestBuilder, Some(checkBuilder))

  override def build(system: ActorSystem, ctx: ScenarioContext, protocolComponentsRegistry: ProtocolComponentsRegistry, next: ActorRef): ActorRef = {
    val hc = httpComponents(protocolComponentsRegistry)
    val request = requestBuilder.build(hc)
    system.actorOf(SseOpenAction.props(requestName, sseName, request, checkBuilder, ctx.coreComponents.statsEngine, hc, next), actorName("sseOpen"))
  }
}

class SseSetCheckActionBuilder(requestName: Expression[String], checkBuilder: WsCheckBuilder, sseName: String) extends HttpActionBuilder {

  def build(system: ActorSystem, ctx: ScenarioContext, protocolComponentsRegistry: ProtocolComponentsRegistry, next: ActorRef): ActorRef =
    system.actorOf(SseSetCheckAction.props(requestName, checkBuilder, sseName, ctx.coreComponents.statsEngine, next), actorName("sseSetCheck"))
}

class SseCancelCheckActionBuilder(requestName: Expression[String], sseName: String) extends HttpActionBuilder {

  def build(system: ActorSystem, ctx: ScenarioContext, protocolComponentsRegistry: ProtocolComponentsRegistry, next: ActorRef): ActorRef =
    system.actorOf(SseCancelCheckAction.props(requestName, sseName, ctx.coreComponents.statsEngine, next), actorName("sseCancelCheck"))
}

class SseReconciliateActionBuilder(requestName: Expression[String], sseName: String) extends HttpActionBuilder {

  override def build(system: ActorSystem, ctx: ScenarioContext, protocolComponentsRegistry: ProtocolComponentsRegistry, next: ActorRef): ActorRef =
    system.actorOf(SseReconciliateAction.props(requestName, sseName, ctx.coreComponents.statsEngine, next), actorName("sseReconciliate"))
}

class SseCloseActionBuilder(requestName: Expression[String], sseName: String) extends HttpActionBuilder {

  override def build(system: ActorSystem, ctx: ScenarioContext, protocolComponentsRegistry: ProtocolComponentsRegistry, next: ActorRef): ActorRef =
    system.actorOf(SseCloseAction.props(requestName, sseName, ctx.coreComponents.statsEngine, next), actorName("sseClose"))
}
