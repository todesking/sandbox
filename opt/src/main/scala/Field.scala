package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import java.lang.reflect.{ Method => JMethod, Field => JField, Modifier }

case class Field(
  classRef: ClassRef,
  name: String,
  descriptor: FieldDescriptor,
  attribute: FieldAttribute,
  value: FieldValue
)
object Field {
  def from(f: JField, obj: AnyRef): Field =
    Field(
      ClassRef.of(f.getDeclaringClass),
      f.getName,
      FieldDescriptor.from(f),
      FieldAttribute.from(f),
      FieldValue.from(f, obj)
    )
}
