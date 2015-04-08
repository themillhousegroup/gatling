/**
 * Copyright 2011-2014 eBusiness Information, Groupe Excilys (www.ebusinessinformation.fr)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.gatling.metrics.types

import scala.collection.mutable

import io.gatling.BaseSpec
import io.gatling.core.config.GatlingConfiguration
import io.gatling.core.result.message.{ OK, KO }
import io.gatling.core.ConfigKeys._

class RequestMetricsBufferSpec extends BaseSpec {

  val configuration = GatlingConfiguration.loadForTest(mutable.Map(
    charting.indicators.Percentile1 -> 95,
    charting.indicators.Percentile2 -> 99,
    http.ahc.RequestTimeout -> 60000))

  def allValues(m: Metrics) = Seq(m.max, m.min, m.percentile1, m.percentile2)

  "RequestMetricsBuffer" should "work when there is no measure" in {
    val buff = new TDigestRequestMetricsBuffer(configuration)
    val metricsByStatus = buff.metricsByStatus

    metricsByStatus.ok shouldBe None
    metricsByStatus.ko shouldBe None
    metricsByStatus.all shouldBe None
  }

  it should "work when there is one OK mesure" in {
    val buff = new TDigestRequestMetricsBuffer(configuration)
    buff.add(OK, 20, Some("200"))

    val metricsByStatus = buff.metricsByStatus
    val okMetrics = metricsByStatus.ok.get

    metricsByStatus.ko shouldBe None
    metricsByStatus.all.map(_.count) shouldBe Some(1l)
    okMetrics.count shouldBe 1L
    all(allValues(okMetrics)) shouldBe 20
    metricsByStatus.statusCodes.size shouldBe 1
    metricsByStatus.statusCodes(Some("200")) shouldBe 1
  }

  it should "work when there are multiple measures" in {
    val buff = new TDigestRequestMetricsBuffer(configuration)
    buff.add(KO, 10, Some("200"))
    val count = 100
    for (t <- 0 until count) buff.add(OK, 100 + t, Some("200"))

    val metricsByStatus = buff.metricsByStatus
    val okMetrics = metricsByStatus.ok.get
    val koMetrics = metricsByStatus.ko.get
    val allMetrics = metricsByStatus.all.get

    koMetrics.count shouldBe 1
    all(allValues(koMetrics)) shouldBe 10
    allMetrics.count shouldBe 101L
    okMetrics.count shouldBe 100L
    okMetrics.min shouldBe 100
    okMetrics.max shouldBe 199
    okMetrics.percentile1 shouldBe 194
    okMetrics.percentile2 shouldBe 198
    metricsByStatus.statusCodes.size shouldBe 1
    metricsByStatus.statusCodes(Some("200")) shouldBe count + 1
  }

  it should "work when there are a large number of measures" in {
    val buff = new TDigestRequestMetricsBuffer(configuration)
    val count = 10000
    for (t <- 1 to count) buff.add(OK, t, Some("200"))

    val metricsByStatus = buff.metricsByStatus
    val okMetrics = metricsByStatus.ok.get

    okMetrics.count shouldBe 10000
    okMetrics.min shouldBe 1
    okMetrics.max shouldBe 10000
    okMetrics.percentile1 shouldBe 9500
    okMetrics.percentile2 shouldBe 9900
    metricsByStatus.statusCodes.size shouldBe 1
    metricsByStatus.statusCodes(Some("200")) shouldBe count
  }
}
