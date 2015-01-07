package com.todesking.scalagp

import scala.util.Random
import scala.reflect.ClassTag

import Ext._

sealed abstract class Definition[A: ClassTag, C, +T <: Tree[A, C]](val name: String, val repository: Repository[C]) extends Equals {
  val klass: ClassTag[A] = implicitly[ClassTag[A]]
  def arity: Int = childClasses.size
  def childClasses: Seq[ClassTag[_]]
  def randomTree(depth: Int)(implicit random: Random): T
  def compatibleShapeDefinitions(): Traversable[Definition[A, C, T]] =
    repository.definitions(klass).filter { d =>
      d.arity == arity && d.childClasses.zip(childClasses).forall { case (a, b) => a.runtimeClass.isAssignableFrom(b.runtimeClass) }
    }.asInstanceOf[Traversable[Definition[A, C, T]]]
  override def canEqual(rhs: Any): Boolean =
    rhs.isInstanceOf[Definition[_, _, _]]
  override def equals(rhs: Any): Boolean = rhs match {
    case d: Definition[A, C, T] =>
      d.canEqual(this) && d.repository == this.repository && d.name == this.name
    case _ => false
  }
}
abstract class BranchDefinition[A: ClassTag, C, +T <: Branch[A, C, _]](name: String, repository: Repository[C]) extends Definition[A, C, T](name, repository) {
  def create(children: Seq[Tree[_, C]]): T

  override def toString() =
    s"(${name} ${childClasses.map(_.runtimeClass.getName).mkString(" ")}) => ${klass.runtimeClass.getName}"
}
abstract class Branch2Definition[A: ClassTag, C, B1, B2, +T <: Branch2[A, C, B1, B2]](name: String, repository: Repository[C]) extends BranchDefinition[A, C, T](name, repository) {
  def unapply(t: Branch2[_, _, _, _]): Option[(Tree[B1, C], Tree[B2, C])] =
    t match {
      case b: Branch2[A, C, B1, B2] if b.definition == this =>
        Some((b.child1, b.child2))
      case _ =>
        None
    }
}
abstract class LeafDefinition[A: ClassTag, C, +T <: Leaf[A, C]](name: String, repository: Repository[C]) extends Definition[A, C, T](name, repository) {
  override val childClasses = Seq.empty
  override def randomTree(depth: Int)(implicit random: Random): T =
    create()
  override def toString() =
    s"(${name}) => ${klass.runtimeClass.getName}"
  def create(): T
}

abstract class ConstLeafDefinition[A: ClassTag, C, +T <: ConstLeaf[A, C]](name: String, repository: Repository[C]) extends LeafDefinition[A, C, T](name, repository) {
  override val childClasses = Seq.empty
  override def randomTree(depth: Int)(implicit random: Random): T =
    create()

  def create(value: A): T
}

class OptimizeDefinition[A: ClassTag, C, D](val name: String, f: (C, D) => A, repository: Repository[C]) {
  def apply(data: D): Tree[A, C] => OptimizedTree[A, C, D] =
    tree => new OptimizedTree[A, C, D](this, tree, data, f)

  def unapply(t: OptimizedTree[_, _, _]): Option[D] =
    t match {
      case ot: OptimizedTree[A, C, D] if ot.realDefinition == this =>
        Some(ot.data)
      case _ =>
        None
    }
}
