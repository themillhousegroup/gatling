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
package io.gatling.charts.stats.reader

import scala.collection.mutable
import io.gatling.core.stats.Group
import io.gatling.core.stats.message.{ KO, Status, MessageEvent }
import io.gatling.core.stats.writer.{ RawErrorRecord, RawGroupRecord, RawUserRecord, RawRequestRecord }

private[reader] class UserRecordParser(bucketFunction: Long => Int, runStart: Long) {

  def unapply(array: Array[String]) = RawUserRecord.unapply(array).map(parseUserRecord)

  private def parseUserRecord(strings: Array[String]): UserRecord = {

    val scenario = strings(1)
    val userId = strings(2)
    val event = MessageEvent(strings(3))
    val startTimestamp = strings(4).toLong
    val endTimestamp = strings(5).toLong

    UserRecord(scenario, userId, event, bucketFunction(startTimestamp), bucketFunction(endTimestamp))
  }
}

private[reader] class RequestRecordParser(bucketFunction: Long => Int, runStart: Long) {

  def unapply(array: Array[String]) = RawRequestRecord.unapply(array).map(parseRequestRecord)

  private def parseRequestRecord(strings: Array[String]): RequestRecord = {

    val group = {
      val groupString = strings(3)
      if (groupString.isEmpty) None else Some(GroupRecordParser.parseGroup(groupString))
    }
    val request = strings(4)

    val firstByteSentTimestamp = strings(5).toLong
    val lastByteSentTimestamp = strings(6).toLong
    val firstByteReceivedTimestamp = strings(7).toLong
    val lastByteReceivedTimestamp = strings(8).toLong

    val status = Status.apply(strings(9))
    val errorMessage = if (status == KO) Some(strings(10)) else None

    val responseTime = (lastByteReceivedTimestamp - firstByteSentTimestamp).toInt
    val latency = (firstByteReceivedTimestamp - lastByteSentTimestamp).toInt

    RequestRecord(group, request, status, bucketFunction(firstByteSentTimestamp), bucketFunction(lastByteReceivedTimestamp), responseTime, latency, errorMessage)
  }
}

private[reader] object GroupRecordParser {

  val GroupCache = mutable.Map.empty[String, Group]

  def parseGroup(string: String) = GroupCache.getOrElseUpdate(string, Group(string.split(",").toList))
}

private[reader] class GroupRecordParser(bucketFunction: Long => Int, runStart: Long) {

  def unapply(array: Array[String]) = RawGroupRecord.unapply(array).map(parseGroupRecord)

  private def parseGroupRecord(strings: Array[String]): GroupRecord = {

    val group = GroupRecordParser.parseGroup(strings(3))
    val startTimestamp = strings(4).toLong
    val endTimestamp = strings(5).toLong
    val cumulatedResponseTime = strings(6).toInt
    val status = Status.apply(strings(7))
    val duration = (endTimestamp - startTimestamp).toInt
    GroupRecord(group, duration, cumulatedResponseTime, status, bucketFunction(startTimestamp))
  }
}

private[reader] object ErrorRecordParser {

  def unapply(array: Array[String]) = RawErrorRecord.unapply(array).map(parseErrorRecord)

  private def parseErrorRecord(strings: Array[String]): ErrorRecord = {

    val message = strings(1)
    val date = strings(2).toLong

    ErrorRecord(message, date)
  }
}

private[reader] case class RequestRecord(group: Option[Group], name: String, status: Status, startBucket: Int, endBucket: Int, responseTime: Int, latency: Int, errorMessage: Option[String])
private[reader] case class GroupRecord(group: Group, duration: Int, cumulatedResponseTime: Int, status: Status, startBucket: Int)
private[reader] case class UserRecord(scenario: String, userId: String, event: MessageEvent, startBucket: Int, endBucket: Int)
private[reader] case class ErrorRecord(message: String, date: Long)
