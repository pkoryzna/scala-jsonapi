package org.zalando.jsonapi.model.instances

import org.zalando.jsonapi.model.{Attribute, Attributes, RootObject}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Poly1, Witness, labelled}
import labelled.FieldType
import shapeless.ops.record._
import org.zalando.jsonapi.model.RootObject.ResourceObject

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

  // todo instance for hlist of key-val pairs for 'only'

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

  def only[A, R <: HList, KS <: HList, OutVS <: HList, OutZip <: HList](keys: KS)(a: A)(implicit
    lg: LabelledGeneric.Aux[A, R],
    selectAll: SelectAll.Aux[R, KS, OutVS],
    zip: shapeless.ops.hlist.ZipWithKeys.Aux[KS, OutVS, OutZip],
    writer: Lazy[AttributesWriter[OutZip]]
  ): Attributes = {
    val rec = lg.to(a)
    val selected = selectAll(rec)
    writer.value.asAttributes(selected.zipWithKeys(ks))
  }

}

trait ResourceObjectWriter[A] {
  def write(a: A)(implicit aw: AttributesWriter[A]): ResourceObject
}
