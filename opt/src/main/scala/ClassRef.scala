package com.todesking.hoge

import scala.language.existentials

sealed abstract class ClassRef {
  def pretty: String
  def name: String
  def classLoader: ClassLoader
  def binaryName: String = name.replaceAll("\\.", "/")
  def <(rhs: ClassRef): Boolean =
    ClassRef.compare(this, rhs).map { case -1 => true; case 0 => false; case 1 => false } getOrElse false

  // TODO: override equals(name and classLoader)
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
    case (l: Concrete, r: Extend) =>
      if(l.loadClass.isAssignableFrom(r.superClass)) Some(1)
      else None
    case (l: Extend, r: Concrete) =>
      if(r.loadClass.isAssignableFrom(l.superClass)) Some(-1)
      else None
    case (l: Extend, r: Extend) =>
      None
  }
  case class Concrete(override val name: String, override val classLoader: ClassLoader) extends ClassRef {
    override def pretty = s"${name}@${System.identityHashCode(classLoader)}"
    // TODO: Is this really correct?
    lazy val loadClass: Class[_] =
      (if(classLoader == null) ClassLoader.getSystemClassLoader else classLoader).loadClass(name)

    def extend(name: String, cl: ClassLoader): Extend =
      Extend(loadClass, name, cl)
  }

  // TODO: interface
  case class Extend(superClass: Class[_], override val name: String, override val classLoader: ClassLoader) extends ClassRef {
    override def pretty = s"${name} <: ${superClass.getName}@${System.identityHashCode(classLoader)}"
    // TODO: Make this REAL unique
    def anotherUniqueName: Extend =
      copy(name = name + "_")
  }

  def of(klass: Class[_]): Concrete =
    ClassRef.Concrete(klass.getName, klass.getClassLoader)
}
