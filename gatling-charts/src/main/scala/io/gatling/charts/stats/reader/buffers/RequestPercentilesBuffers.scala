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
package io.gatling.charts.stats.reader.buffers

import scala.collection.mutable
import io.gatling.charts.stats.reader.RequestRecord
import io.gatling.core.stats.Group
import io.gatling.core.stats.message.Status

private[reader] trait RequestPercentilesBuffers {
  this: Buckets =>

  val requestPercentilesBuffers = mutable.Map.empty[BufferKey, (PercentilesBuffers, PercentilesBuffers)]

  private def percentilesBufferPair(requestName: Option[String], group: Option[Group], status: Status): (PercentilesBuffers, PercentilesBuffers) =
    requestPercentilesBuffers.getOrElseUpdate(BufferKey(requestName, group, Some(status)), (new PercentilesBuffers(buckets), new PercentilesBuffers(buckets)))

  def getResponseTimePercentilesBuffers(requestName: Option[String], group: Option[Group], status: Status): PercentilesBuffers =
    percentilesBufferPair(requestName, group, status)._1

  def getLatencyPercentilesBuffers(requestName: Option[String], group: Option[Group], status: Status): PercentilesBuffers =
    percentilesBufferPair(requestName, group, status)._2

  private def updateRequestPercentilesBuffers(requestName: Option[String], group: Option[Group], status: Status, requestStartBucket: Int, responseTime: Int, latency: Int): Unit = {
    val (responseTimeHistogramBuffers, latencyHistogramBuffers) = percentilesBufferPair(requestName, group, status)
    responseTimeHistogramBuffers.update(requestStartBucket, responseTime)
    latencyHistogramBuffers.update(requestStartBucket, latency)
  }

  def updateRequestPercentilesBuffers(record: RequestRecord): Unit = {
    import record._
    updateRequestPercentilesBuffers(Some(name), group, status, startBucket, responseTime, latency)
    updateRequestPercentilesBuffers(None, None, status, startBucket, responseTime, latency)
  }
}
