package com.todesking.scalagp

import scala.util.Random
import scala.reflect.ClassTag

import Ext._

sealed abstract class Tree[A, C] extends Equals {
  def definition: Definition[A, C, Tree[A, C]]
  def apply(ctx: C): A
  def height: Int
  def size: Int
  def isConstant: Boolean = false
  def optimized: Tree[A, C] =
    definition.repository.optimize(this)
  def allPaths: Traversable[TreePath[A, C, _]] =
    allPaths(TreePath.Root(this))
  def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]]
  def randomPath()(implicit random: Random): TreePath[A, C, _] =
    allPaths.toSeq.sample().get
  def randomPath[X](klass: ClassTag[X])(implicit random: Random): Option[TreePath[A, C, X]] = {
    allPaths.filter { p =>
      klass.runtimeClass.isAssignableFrom(p.value.definition.klass.runtimeClass)
    }.toSeq.sample().map(_.asInstanceOf[TreePath[A, C, X]])
  }

  override def canEqual(rhs: Any): Boolean =
    rhs.isInstanceOf[Tree[_, _]]
}

class OptimizedTree[A, C, D](val realDefinition: OptimizeDefinition[A, C, D], val wrapped: Tree[A, C], val data: D, val f: (C, D) => A) extends Tree[A, C] {
  override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]] =
    wrapped.allPaths(base)
  override def definition: Definition[A, C, Tree[A, C]] =
    wrapped.definition
  override def height =
    wrapped.height
  override def size =
    wrapped.size
  def apply(ctx: C): A =
    f(ctx, data)
  override def toString =
    s"[${realDefinition.name} ${data}]${wrapped.toString}"
}

sealed abstract class Leaf[A, C] extends Tree[A, C] {
  override def definition: LeafDefinition[A, C, Leaf[A, C]]
  override val height = 0
  override val size = 1
  override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, A]] =
    Seq(base)
}
object Leaf {
  def unapply[A, C](l: Leaf[A, C]): Option[ClassTag[A]] =
    Some(l.definition.klass)
}
class ConstLeaf[A, C](val value: A, override val definition: ConstLeafDefinition[A, C, ConstLeaf[A, C]]) extends Leaf[A, C] {
  override def isConstant = true
  override def apply(ctx: C): A = value
  override def toString =
    s"(${definition.name} ${value.toString})"

  override def equals(rhs: Any): Boolean = rhs match {
    case t: ConstLeaf[A, C] @unchecked =>
      t.canEqual(this) && t.definition == this.definition && t.value == this.value
    case _ => false
  }
  override def hashCode =
    definition.hashCode ^ value.hashCode
}
object ConstLeaf {
  def unapply[A, C](cl: ConstLeaf[A, C]): Option[(ClassTag[A], A)] =
    Some(cl.definition.klass -> cl.value)
}
class FunctionLeaf[A, C](val function: C => A, override val definition: LeafDefinition[A, C, FunctionLeaf[A, C]]) extends Leaf[A, C] {
  override def apply(ctx: C): A =
    function(ctx)
  override def toString =
    s"(${definition.name})"
  override def equals(rhs: Any): Boolean = rhs match {
    case t: FunctionLeaf[A, C] @unchecked => t.canEqual(this) && t.definition == this.definition
    case _ => false
  }
  override def hashCode =
    definition.hashCode
}
sealed abstract class Branch[A, C, P] extends Tree[A, C] {
  def children: Seq[Tree[_, C]]
  protected def apply0(ctx: C): A

  override lazy val isConstant = children.forall(_.isConstant)

  override def definition: BranchDefinition[A, C, Branch[A, C, P]]
  override lazy val height = children.map(_.height).max + 1
  override lazy val size = children.map(_.size).sum + 1

  private[this] var _cached: Any = null
  override def apply(ctx: C): A =
    if(isConstant && _cached != null) {
      _cached.asInstanceOf[A]
    } else {
      _cached = apply0(ctx)
      _cached.asInstanceOf[A]
    }

  def replaceChild(index: Int, c: Tree[_, C]): Branch[A, C, P] =
    definition.create(children.updated(index, c))

  override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]] =
    base +: children.zipWithIndex.flatMap { case (c: Tree[_, C], i) => c.allPaths(TreePath.Descent(base, i, c)) }

  override def toString =
    s"(${definition.name} ${children.map(_.toString).mkString(" ")})"

  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: Branch[A, C, P] @unchecked => rhs.canEqual(this) && rhs.children == this.children
    case _ => false
  }

  override def hashCode =
    children.foldLeft(definition.hashCode) { (a, x) => a ^ x.hashCode }
}
class Branch2[A, C, B1, B2](
  override val definition: BranchDefinition[A, C, Branch2[A, C, B1, B2]],
  val child1: Tree[B1, C],
  val child2: Tree[B2, C],
  val function: (C, Tree[B1, C], Tree[B2, C]) => A
) extends Branch[A, C, (Tree[B1, C], Tree[B2, C])] {
  override val children = Seq(child1, child2)
  override def apply0(ctx: C): A =
    function(ctx, child1, child2)
}
object Branch {
  def unapply[A, C, P](b: Branch[A, C, P]): Option[ClassTag[A]] =
    Some(b.definition.klass)
}

