package org.zalando.jsonapi.model.implicits

import org.scalatest.{Matchers, WordSpec}
import org.zalando.jsonapi.model.Attribute
import org.zalando.jsonapi.model.JsonApiObject._
import org.zalando.jsonapi.model.implicits.JsonApiObjectValueConversions._

class JsonApiObjectValueConversionsSpec extends WordSpec with Matchers {
  def convertAnyToValue[A](a: A)(implicit convertToValue: ConvertToValue[A]) = convertToValue(a)

  "scala values" should {
    "be converted to string values" in {
      convertAnyToValue("string") should be(StringValue("string"))
    }
    "be converted to number values" in {
      convertAnyToValue(42) should be(NumberValue(42))
      convertAnyToValue(42l) should be(NumberValue(42))
      convertAnyToValue(42f) should be(NumberValue(42))
      convertAnyToValue(42d) should be(NumberValue(42))
    }
    "be converted to boolean values" in {
      convertAnyToValue(true) should be(TrueValue)
      convertAnyToValue(false) should be(FalseValue)
    }
    // todo why would we even allow to compile this???

//    "be converted to js array values" in {
//      convertAnyToValue(Seq("one", 2, Map("3" → 4d), false, null)) should be(JsArrayValue(List(
//        StringValue("one"),
//        NumberValue(2),
//        JsObjectValue(List(Attribute("3", NumberValue(4d)))),
//        FalseValue,
//        NullValue
//      )))
//    }
    // todo why would we even allow to compile this???
//    "be converted to js object values" in {
//      convertAnyToValue(Map(
//        "one" → 2,
//        "3" → List(4f, true, null)
//      )) should be(JsObjectValue(List(
//        Attribute("one", NumberValue(2)),
//        Attribute("3", JsArrayValue(List(
//          NumberValue(4f),
//          TrueValue,
//          NullValue
//        )))
//      )))
//    }
    "be converted to null values" in {
      convertAnyToValue(null) should be(NullValue)
    }
    "be left alone" in {
      for {
        value ← Seq(NullValue, TrueValue, FalseValue, StringValue("value"), NumberValue(1))
      } {
        convertAnyToValue(value) should be (value)
      }
    }
    "not allow unconvertible types" in {
      """convertAnyToValue(Map(1 → 2))""" shouldNot compile

      """convertAnyToValue(new java.util.Date)""" shouldNot compile
    }
  }
}
