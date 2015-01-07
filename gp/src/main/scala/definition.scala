package com.todesking.scalagp

import scala.util.Random
import Ext._

sealed abstract class Definition[A, C, +T <: Tree[A, C]](val name: String, val klass: Class[_ <: A], val repository: Repository[C]) extends Equals {
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
abstract class Branch2Definition[A, C, B1, B2, +T <: Branch2[A, C, B1, B2]](name: String, klass: Class[_ <: A], repository: Repository[C]) extends BranchDefinition[A, C, T](name, klass, repository) {
  def unapply(t: Branch2[_, _, _, _]): Option[(Tree[B1, C], Tree[B2, C])] =
    t match {
      case b: Branch2[A, C, B1, B2] if b.definition == this =>
        Some((b.child1, b.child2))
      case _ =>
        None
    }
}
abstract class LeafDefinition[A, C, +T <: Leaf[A, C]](name: String, klass: Class[_ <: A], repository: Repository[C]) extends Definition[A, C, T](name, klass, repository) {
  override val childClasses = Seq.empty
  override def randomTree(repository: Repository[C], depth: Int)(implicit random: Random): T =
    create()
  override def toString() =
    s"(${name}) => ${klass.getName}"
  def create(): T
}

abstract class FunctionLeafDefinition[A, C, +T <: FunctionLeaf[A, C]](name: String, klass: Class[_ <: A], repository: Repository[C]) extends LeafDefinition[A, C, T](name, klass, repository) {
  def unapply(l: Leaf[_, C]): Boolean = l match {
    case l: Leaf[A, C] if l.definition == this =>
      true
    case _ =>
      false
  }
}

abstract class ConstLeafDefinition[A, C, +T <: ConstLeaf[A, C]](name: String, klass: Class[_ <: A], repository: Repository[C]) extends LeafDefinition[A, C, T](name, klass, repository) {
  override val childClasses = Seq.empty
  override def randomTree(repository: Repository[C], depth: Int)(implicit random: Random): T =
    create()

  def create(value: A): T

  def unapply(l: ConstLeaf[_, _]): Option[A] = l match {
    case cl: ConstLeaf[A, C] if cl.definition == this =>
      Some(cl.value)
    case _ =>
      None
  }
}

class OptimizeDefinition[A, C, D](val name: String, val klass: Class[_ <: A], f: (C, D) => A, repository: Repository[C]) {
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
