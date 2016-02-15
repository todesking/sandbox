package com.todesking.hoge

case class ClassRef(str: String, classLoader: java.lang.ClassLoader) {
  def binaryString: String = str.replaceAll("\\.", "/")
  def loadClass: Class[_] = classLoader.loadClass(str)
}

object ClassRef {
  def of(klass: Class[_]): ClassRef =
    ClassRef(klass.getName, klass.getClassLoader)
}
