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
package io.gatling.core.result.writer

import io.gatling.core.assertion.Assertion
import io.gatling.core.config.GatlingConfiguration
import io.gatling.core.result.message.{ ResponseTimings, MessageEvent, Status }
import io.gatling.core.session.Session

case class ShortScenarioDescription(name: String, nbUsers: Int)

case class RunMessage(simulationClassName: String,
                      simulationId: String,
                      start: Long,
                      runDescription: String) {

  val runId = simulationId + "-" + start
}

sealed trait DataWriterMessage
case class Init(configuration: GatlingConfiguration, assertions: Seq[Assertion], runMessage: RunMessage, scenarios: Seq[ShortScenarioDescription]) extends DataWriterMessage
case object Flush extends DataWriterMessage
case class Crash(cause: String) extends DataWriterMessage
case object Terminate extends DataWriterMessage

sealed trait LoadEventMessage extends DataWriterMessage

case class UserMessage(
  session: Session,
  event: MessageEvent,
  date: Long) extends LoadEventMessage

case class RequestMessage(
  scenario: String,
  userId: String,
  groupHierarchy: List[String],
  name: String,
  start: Long) extends LoadEventMessage

case class ResponseMessage(
  scenario: String,
  userId: String,
  groupHierarchy: List[String],
  name: String,
  timings: ResponseTimings,
  status: Status,
  responseCode: Option[String],
  message: Option[String],
  extraInfo: List[Any]) extends LoadEventMessage

case class GroupMessage(
    scenario: String,
    userId: String,
    groupHierarchy: List[String],
    startDate: Long,
    endDate: Long,
    cumulatedResponseTime: Int,
    status: Status) extends LoadEventMessage {
  val duration = (endDate - startDate).toInt
}

case class ErrorMessage(message: String, date: Long) extends LoadEventMessage
