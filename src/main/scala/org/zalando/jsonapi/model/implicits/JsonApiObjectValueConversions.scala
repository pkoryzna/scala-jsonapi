package org.zalando.jsonapi.model.implicits

import scala.language.implicitConversions
import org.zalando.jsonapi.model.Attribute
import org.zalando.jsonapi.model.JsonApiObject._

object JsonApiObjectValueConversions {

  type ConvertToValue[A] = A => Value

  private def nullConv: PartialFunction[Any, Value] = {
    case null => NullValue
  }

  private def makeConv[From](f: From => Value): ConvertToValue[From] = nullConv.orElse(PartialFunction(f))

  implicit val string2Value: ConvertToValue[String] = makeConv { s: String => StringValue(s) }
  implicit val boolean2Value = makeConv[Boolean] {
    case true  => TrueValue
    case false => FalseValue
  }

  implicit val float2Value: ConvertToValue[Float] = makeConv { f => NumberValue(BigDecimal(f)) }
  implicit val double2Value: ConvertToValue[Double] = makeConv { d => NumberValue(BigDecimal(d)) }
  implicit val int2Value: ConvertToValue[Int] = makeConv { int => NumberValue(BigDecimal(int)) }
  implicit val long2Value: ConvertToValue[Long] = makeConv { long => NumberValue(BigDecimal(long)) }

  implicit def map2Value[K <: String, V](implicit conv: ConvertToValue[V]): ConvertToValue[Map[K, V]] =
    makeConv { (m: Map[K, V]) =>
     val attrs = m.map { case (k, v) => Attribute(k, conv(v)) }
      JsObjectValue(attrs.toList)
    }

  implicit def seq2Value[A](implicit conv: ConvertToValue[A]) = makeConv { (s: Seq[A]) =>
    JsArrayValue(s.map(conv))
  }

}