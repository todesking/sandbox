package com.todesking.hoge

case class ClassRef(str: String, classLoader: java.lang.ClassLoader) {
  // TODO
  def name: String = str
  def binaryName = binaryString
  def binaryString: String = str.replaceAll("\\.", "/")
  // TODO: Is this really correct?
  lazy val loadClass: Class[_] =
    (if(classLoader == null) ClassLoader.getSystemClassLoader else classLoader).loadClass(str)

  def <(rhs: ClassRef): Boolean =
    loadClass != rhs.loadClass && rhs.loadClass.isAssignableFrom(loadClass)
}

object ClassRef {
  def of(klass: Class[_]): ClassRef =
    ClassRef(klass.getName, klass.getClassLoader)
  def newAnonymous(parentCL: ClassLoader, namePrefix: String = "anonymous_"): ClassRef =
    ClassRef(newAnonymousClassName(namePrefix), new java.net.URLClassLoader(Array.empty, parentCL))

  private[this] var anonId: Int = 0
  private[this] def newAnonymousClassName(namePrefix: String): String = synchronized {
    anonId += 1
    namePrefix + anonId.toString
  }
}
