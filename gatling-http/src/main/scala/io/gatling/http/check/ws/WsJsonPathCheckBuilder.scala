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
package io.gatling.http.check.ws

import io.gatling.core.check.{ DefaultMultipleFindCheckBuilder, Extender }
import io.gatling.core.json.JsonParsers
import io.gatling.core.check.extractor.jsonpath._
import io.gatling.core.session.Expression

trait WsJsonPathOfType {
  self: WsJsonPathCheckBuilder[String] =>

  def ofType[X: JsonFilter](implicit extractorFactory: JsonPathExtractorFactory) = new WsJsonPathCheckBuilder[X](path, extender, jsonParsers)
}

object WsJsonPathCheckBuilder {

  def jsonPath(path: Expression[String], extender: Extender[WsCheck, String])(implicit extractorFactory: JsonPathExtractorFactory, jsonParsers: JsonParsers) =
    new WsJsonPathCheckBuilder[String](path, extender, jsonParsers) with WsJsonPathOfType
}

class WsJsonPathCheckBuilder[X: JsonFilter](private[ws] val path: Expression[String],
                                            private[ws] val extender: Extender[WsCheck, String],
                                            private[ws] val jsonParsers: JsonParsers)(implicit extractorFactory: JsonPathExtractorFactory)
    extends DefaultMultipleFindCheckBuilder[WsCheck, String, Any, X](
      extender,
      jsonParsers.safeParse) {

  import extractorFactory._

  def findExtractor(occurrence: Int) = path.map(newSingleExtractor[X](_, occurrence))
  def findAllExtractor = path.map(newMultipleExtractor[X])
  def countExtractor = path.map(newCountExtractor)
}
