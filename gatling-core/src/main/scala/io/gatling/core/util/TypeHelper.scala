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
package io.gatling.core.util

import io.gatling.core.NotNothing

import scala.reflect.ClassTag

import io.gatling.core.validation.{ FailureWrapper, SuccessWrapper, Validation }

object TypeHelper {

  val NullValueFailure = "Value is null".failure

  def typeMatches[T: ClassTag](obj: Any) =
    obj.getClass.isAssignableFrom(implicitly[ClassTag[T]].runtimeClass)

  implicit class TypeCaster(val value: Any) extends AnyVal {

    private def valueClazz = value.getClass.getName match {
      case "java.lang.Boolean"   => classOf[Boolean]
      case "java.lang.Byte"      => classOf[Byte]
      case "java.lang.Short"     => classOf[Short]
      case "java.lang.Integer"   => classOf[Int]
      case "java.lang.Long"      => classOf[Long]
      case "java.lang.Float"     => classOf[Float]
      case "java.lang.Double"    => classOf[Double]
      case "java.lang.Character" => classOf[Char]
      case _                     => value.getClass
    }

    private def cceMessage(clazz: Class[_]) = s"Can't cast value $value of type ${value.getClass} into $clazz"

    def asOption[T: ClassTag: NotNothing]: Option[T] = Option(value) match {
      case Some(v) =>
        val clazz = implicitly[ClassTag[T]].runtimeClass

        if (clazz == classOf[String])
          Some(value.toString.asInstanceOf[T])
        else if (clazz.isAssignableFrom(value.getClass) || clazz.isAssignableFrom(valueClazz))
          Some(value.asInstanceOf[T])
        else
          throw new ClassCastException(cceMessage(clazz))
      case _ => throw new ClassCastException(NullValueFailure.message)
    }

    def asValidation[T: ClassTag: NotNothing]: Validation[T] = Option(value) match {
      case Some(v) =>
        val clazz = implicitly[ClassTag[T]].runtimeClass

        if (clazz == classOf[String])
          value.toString.asInstanceOf[T].success
        else if (clazz.isAssignableFrom(value.getClass) || clazz.isAssignableFrom(valueClazz))
          value.asInstanceOf[T].success
        else
          cceMessage(clazz).failure

      case _ => NullValueFailure
    }
  }
}
