package com.todesking.scalagp

import scala.util.Random
import Ext._

sealed abstract class Tree[A, C] {
  def definition: Definition[A, C, Tree[A, C]]
  def apply(ctx: C): A
  val height: Int
  val size: Int
  def isConstant: Boolean = false
  def allPaths: Traversable[TreePath[A, C, _]] =
    allPaths(TreePath.Root(this))
  def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]]
  def randomPath()(implicit random: Random): TreePath[A, C, _] =
    allPaths.toSeq.sample().get
  def randomPath[X](klass: Class[X])(implicit random: Random): Option[TreePath[A, C, X]] = {
    allPaths.filter { p =>
      klass.isAssignableFrom(p.value.definition.klass)
    }.toSeq.sample().map(_.asInstanceOf[TreePath[A, C, X]])
  }
}
abstract class Leaf[A, C] extends Tree[A, C] {
  override def definition: LeafDefinition[A, C, Leaf[A, C]]
  override val height = 0
  override val size = 1
  override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, A]] =
    Seq(base)
}
object Leaf {
  def unapply[A, C](l: Leaf[A, C]): Option[Class[_ <: A]] =
    Some(l.definition.klass)
}
class ConstLeaf[A, C](val value: A, override val definition: LeafDefinition[A, C, ConstLeaf[A, C]]) extends Leaf[A, C] {
  override def isConstant = true
  override def apply(ctx: C): A = value
  override def toString =
    value.toString
}
object ConstLeaf {
  def unapply[A, C](cl: ConstLeaf[A, C]): Option[(Class[_ <: A], A)] =
    Some(cl.definition.klass -> cl.value)
}
class FunctionLeaf[A, C](val function: C => A, override val definition: LeafDefinition[A, C, FunctionLeaf[A, C]]) extends Leaf[A, C] {
  override def apply(ctx: C): A =
    function(ctx)
  override def toString =
    s"(${definition.name})"
}
sealed abstract class Branch[A, C, P](
  val function: ((C, P)) => A
) extends Tree[A, C] {
  def children: Seq[Tree[_, C]]
  def childrenParam: P

  override lazy val isConstant = children.forall(_.isConstant)

  override def definition: BranchDefinition[A, C, Branch[A, C, P]]
  override lazy val height = children.map(_.height).max
  override lazy val size = children.map(_.size).sum + 1

  private[this] var _cached: Any = null
  override def apply(ctx: C): A =
    if(isConstant && _cached != null) {
      _cached.asInstanceOf[A]
    } else {
      _cached = function((ctx, childrenParam))
      _cached.asInstanceOf[A]
    }

  def replaceChild(index: Int, c: Tree[_, C]): Branch[A, C, P] =
    definition.create(children.updated(index, c))

  override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]] =
    base +: children.zipWithIndex.flatMap { case (c: Tree[_, C], i) => c.allPaths(TreePath.Descent(base, i, c)) }

  override def toString =
    s"(${definition.name} ${children.map(_.toString).mkString(" ")})"
}
class Branch2[A, C, B1, B2](
  override val definition: BranchDefinition[A, C, Branch2[A, C, B1, B2]],
  val child1: Tree[B1, C],
  val child2: Tree[B2, C],
  function: ((C,(Tree[B1, C], Tree[B2, C]))) => A
) extends Branch[A, C, (Tree[B1, C], Tree[B2, C])](function) {
  override val children = Seq(child1, child2)
  override val childrenParam = (child1, child2)
}
object Branch {
  def unapply[A, C, P](b: Branch[A, C, P]): Option[Class[_ <: A]] =
    Some(b.definition.klass)
}

