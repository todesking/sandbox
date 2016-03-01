package com.todesking.hoge

case class FieldRef(name: String, descriptor: FieldDescriptor) {
  def pretty: String = s"${name}: ${descriptor.str}"
}
object FieldRef {
  def from(f: java.lang.reflect.Field): FieldRef =
    FieldRef(f.getName, FieldDescriptor.from(f))
}
