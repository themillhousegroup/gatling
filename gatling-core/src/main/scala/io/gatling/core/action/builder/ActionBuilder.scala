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
package io.gatling.core.action.builder

import akka.actor.{ ActorSystem, ActorRef }
import io.gatling.core.akka.ActorNames
import io.gatling.core.protocol.ProtocolComponentsRegistry
import io.gatling.core.structure.ScenarioContext

/**
 * Top level abstraction for components in charge of building Actions.
 * ActionBuilder is what is passed to the DSL exec() method.
 */
trait ActionBuilder extends ActorNames {

  /**
   * @param system the ActorSystem
   * @param next the Action that will be chained with the Action build by this builder
   * @param ctx the scenario context
   * @param protocolComponentsRegistry the ProtocolComponents registry
   * @return the resulting Action actor
   */
  def build(system: ActorSystem, ctx: ScenarioContext, protocolComponentsRegistry: ProtocolComponentsRegistry, next: ActorRef): ActorRef
}
