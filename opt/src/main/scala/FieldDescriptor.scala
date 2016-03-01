package com.todesking.hoge

case class FieldDescriptor(typeRef: TypeRef.Public) {
  def str = typeRef.str
}
object FieldDescriptor {
  def from(f: java.lang.reflect.Field): FieldDescriptor =
    FieldDescriptor(TypeRef.from(f.getType))
  def parse(src: String, cl: ClassLoader): FieldDescriptor =
    Parsers.parseFieldDescriptor(src, cl)
}