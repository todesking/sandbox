package com.todesking.hoge

// TODO: Add ClassRef
case class FieldRef(name: String, descriptor: FieldDescriptor) {
  def str: String = s"${name}: ${descriptor.str}"
}
