package com.todesking.hoge

case class LocalFieldRef(name: String, descriptor: FieldDescriptor) {
  def str: String = s"${name}: ${descriptor.str}"
}
