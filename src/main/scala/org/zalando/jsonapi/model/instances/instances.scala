package org.zalando.jsonapi.model.instances

import org.zalando.jsonapi.model.{Attribute, Attributes}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}
import labelled.FieldType

trait AttributesWriter[A] {
  /**
   * Transforms an object to a list of attributes
   */
  def asAttributes(a: A): Attributes
}

object AttributesWriter {

  import org.zalando.jsonapi.model.implicits.JsonApiObjectValueConversions._

  implicit def attributesForField[K <: Symbol, V](implicit
    witness: Witness.Aux[K],
    valueConv: ConvertToValue[V]
  ): AttributesWriter[FieldType[K, V]] = createWriter { v: FieldType[K, V] =>
    import org.zalando.jsonapi.model.implicits.AttributeConversions._

    List(
      witness.value.name -> valueConv(v)
    )
  }

  implicit def attributesForFieldOpt[K <: Symbol, V](implicit
    witness: Witness.Aux[K],
    valueConv: ConvertToValue[V]
  ): AttributesWriter[FieldType[K, Option[V]]] = createWriter { v: FieldType[K, Option[V]] =>
    import org.zalando.jsonapi.model.implicits.AttributeConversions._

    convertPairToOptionalAttribute(witness.value.name -> v).toList
  }

  implicit val hnilWriter: AttributesWriter[HNil] = createWriter { hn: HNil =>
    List.empty[Attribute]
  }

  implicit def hlistWriter[K <: Symbol, H, T <: HList](
    implicit
    hWriter: Lazy[AttributesWriter[FieldType[K, H]]],
    tWriter: AttributesWriter[T]
  ): AttributesWriter[FieldType[K, H] :: T] = createWriter {
    case h :: t =>
      hWriter.value.asAttributes(h) ++ tWriter.asAttributes(t)
  }


  def createWriter[A](f: A => Attributes): AttributesWriter[A] = new AttributesWriter[A] {
    override def asAttributes(a: A) = f(a)
  }



  /**
   * Serializes whole object as Attributes.
   */
  def whole[A, R <: HList](a: A)(implicit lg: LabelledGeneric.Aux[A, R], writer: Lazy[AttributesWriter[R]]): Attributes = {
    writer.value.asAttributes(lg.to(a))
  }

}


