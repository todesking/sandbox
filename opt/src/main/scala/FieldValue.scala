package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import java.lang.reflect.{ Method => JMethod , Field => JField, Modifier}


sealed abstract class FieldValue {
  def value: Any
}
object FieldValue {
  def from(f: JField, obj: AnyRef): FieldValue = {
    val v = f.get(obj)
    TypeRef.from(f.getType) match {
      case _: TypeRef.Primitive => Primitive(v.asInstanceOf[AnyVal])
      case _: TypeRef.Reference if v == null => Null
      case _: TypeRef.Reference => Reference(Instance.of(v))
    }
  }

  case class Primitive(override val value: AnyVal) extends FieldValue
  case class Reference(instance: Instance[_ <: AnyRef]) extends FieldValue {
    override def value = instance.materialized.value
  }
  case object Null extends FieldValue {
    override val value = null
  }
}

