package com.todesking.hoge

import scala.language.existentials

sealed abstract class ClassRef {
  def pretty: String
  def <(rhs: ClassRef): Boolean =
    ClassRef.compare(this, rhs).map { case -1 => true; case 0 => false; case 1 => false } getOrElse false
}
object ClassRef {
  // Some(n): Determinable
  // None: Not sure
  def compare(lhs: ClassRef, rhs: ClassRef): Option[Int] = (lhs, rhs) match {
    case (l: Concrete, r: Concrete) =>
      if(l.loadClass == r.loadClass) Some(0)
      else if(l.loadClass.isAssignableFrom(r.loadClass)) Some(1)
      else if(r.loadClass.isAssignableFrom(l.loadClass)) Some(-1)
      else None
    case (l: Concrete, r: SomeRef) =>
      if(l.loadClass.isAssignableFrom(r.superClass)) Some(1)
      else None
    case (l: SomeRef, r: Concrete) =>
      if(r.loadClass.isAssignableFrom(l.superClass)) Some(-1)
      else None
    case (l: SomeRef, r: SomeRef) =>
      None
  }
  case class Concrete(name: String, classLoader: java.lang.ClassLoader) extends ClassRef {
    override def pretty = s"${name}@${System.identityHashCode(classLoader)}"
    def str = name
    def binaryName = binaryString
    def binaryString: String = name.replaceAll("\\.", "/")
    // TODO: Is this really correct?
    lazy val loadClass: Class[_] =
      (if(classLoader == null) ClassLoader.getSystemClassLoader else classLoader).loadClass(name)

    def someSubclassRef(cl: ClassLoader): SomeRef =
      SomeRef(loadClass, cl)
  }

  // TODO: interface
  case class SomeRef(superClass: Class[_], classLoader: ClassLoader) extends ClassRef {
    override def pretty = s"_ <: ${superClass.getName}@${System.identityHashCode(classLoader)}"
  }

  def of(klass: Class[_]): Concrete =
    ClassRef.Concrete(klass.getName, klass.getClassLoader)

  def some(superClass: Class[_], cl: ClassLoader): SomeRef =
    ClassRef.SomeRef(superClass, cl)

  // TODO: remove
  def newAnonymous(parentCL: ClassLoader, namePrefix: String = "anonymous_"): ClassRef =
    ClassRef.Concrete(newAnonymousClassName(namePrefix), new java.net.URLClassLoader(Array.empty, parentCL))

  private[this] var anonId: Int = 0
  private[this] def newAnonymousClassName(namePrefix: String): String = synchronized {
    anonId += 1
    namePrefix + anonId.toString
  }
}
