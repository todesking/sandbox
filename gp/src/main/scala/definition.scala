package com.todesking.scalagp

import scala.util.Random
import Ext._

abstract class Definition[A, C, +T <: Tree[A, C]](val name: String, val klass: Class[_ <: A], val repository: Repository[C]) extends Equals {
  def arity: Int = childClasses.size
  def childClasses: Seq[Class[_]]
  def randomTree(repository: Repository[C], depth: Int)(implicit random: Random): T
  def compatibleShapeDefinitions(): Traversable[Definition[A, C, T]] =
    repository.definitions(klass).filter { d =>
      d.arity == arity && d.childClasses.zip(childClasses).forall { case (a, b) => a.isAssignableFrom(b) }
    }.asInstanceOf[Traversable[Definition[A, C, T]]]
  override def canEqual(rhs: Any): Boolean =
    rhs.isInstanceOf[Definition[_, _, _]]
  override def equals(rhs: Any): Boolean = rhs match {
    case d: Definition[A, C, T] =>
      d.canEqual(this) && d.repository == this.repository && d.name == this.name
    case _ => false
  }
}
abstract class BranchDefinition[A, C, +T <: Branch[A, C, _]](name: String, klass: Class[_ <: A], repository: Repository[C]) extends Definition[A, C, T](name, klass, repository) {
  def create(children: Seq[Tree[_, C]]): T
  override def toString() =
    s"(${name} ${childClasses.map(_.getName).mkString(" ")}) => ${klass.getName}"
}
abstract class LeafDefinition[A, C, +T <: Leaf[A, C]](name: String, klass: Class[_ <: A], repository: Repository[C]) extends Definition[A, C, T](name, klass, repository) {
  override val childClasses = Seq.empty
  def create(): T
  override def randomTree(repository: Repository[C], depth: Int)(implicit random: Random): T =
    create()
  override def toString() =
    s"(${name}) => ${klass.getName}"
}
