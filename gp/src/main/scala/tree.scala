package com.todesking.scalagp

import scala.util.Random
import scala.reflect.ClassTag

import Ext._

sealed abstract class Definition[A: ClassTag, C](val name: String, val repository: Repository[C]) extends Equals {
  type TREE <: Tree[A, C]
  type CompatibleType <: Definition[A, C]

  val klass: ClassTag[A] = implicitly[ClassTag[A]]
  def arity: Int = childClasses.size
  def childClasses: Seq[ClassTag[_]]
  def randomTree(depth: Int)(implicit random: Random): TREE
  def compatibleShapeDefinitions(): Seq[CompatibleType] =
    repository.definitions(klass).filter(isCompatible(_)).map(_.asInstanceOf[CompatibleType]).toSeq
  def isCompatible(d: Definition[A, C]): Boolean =
    d.arity == arity && d.childClasses.zip(childClasses).forall { case (a, b) => a.runtimeClass.isAssignableFrom(b.runtimeClass) }
  override def canEqual(rhs: Any): Boolean =
    rhs.isInstanceOf[Definition[_, _]]
  override def equals(rhs: Any): Boolean = rhs match {
    case d: Definition[A, C] =>
      d.canEqual(this) && d.repository == this.repository && d.name == this.name
    case _ => false
  }
}

sealed abstract class Tree[A, C] extends Equals {
  def definition: Definition[A, C]
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

sealed class OptimizeDefinition[A: ClassTag, C, D](val name: String, f: (C, D) => A, repository: Repository[C]) {
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

sealed class OptimizedTree[A, C, D](
  val realDefinition: OptimizeDefinition[A, C, D],
  val wrapped: Tree[A, C],
  val data: D, val f: (C, D) => A
) extends Tree[A, C] {
  override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]] =
    wrapped.allPaths(base)
  override def definition: Definition[A, C] =
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

sealed abstract class LeafDefinition[A: ClassTag, C](
  name: String,
  repository: Repository[C]
) extends Definition[A, C](name, repository) {
  override type TREE <: Leaf[A, C]
  override type CompatibleType = LeafDefinition[A, C]
  override val childClasses = Seq.empty
  override def randomTree(depth: Int)(implicit random: Random): TREE =
    create()
  override def toString() =
    s"(${name}) => ${klass.runtimeClass.getName}"
  def create(): TREE
}

sealed abstract class Leaf[A, C] extends Tree[A, C] {
  override def definition: LeafDefinition[A, C]
  override val height = 0
  override val size = 1
  override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, A]] =
    Seq(base)
}

sealed class ConstLeafDefinition[A: ClassTag, C](
  name: String,
  repository: Repository[C],
  generateValue: () => A
) extends LeafDefinition[A, C](name, repository) {
  override type TREE = ConstLeaf[A, C]
  override val childClasses = Seq.empty
  override def randomTree(depth: Int)(implicit random: Random): ConstLeaf[A, C] =
    create()
  override def create() =
    create(generateValue())
  def create(value: A) =
    new ConstLeaf[A, C](value, this)
  def unapply[AX, CX](leaf: ConstLeaf[AX, CX]): Option[A] =
    if(leaf.definition == this)
      Some(leaf.asInstanceOf[ConstLeaf[A, C]].value)
    else
      None
}

sealed class ConstLeaf[A, C](val value: A, override val definition: ConstLeafDefinition[A, C]) extends Leaf[A, C] {
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

sealed class FunctionLeafDefinition[A: ClassTag, C](
  name: String,
  repository: Repository[C],
  function: C => A
) extends LeafDefinition[A, C](name, repository) {
  override type TREE = FunctionLeaf[A, C]
  override def create() = new FunctionLeaf[A, C](function, this)

  def unapply[AX, CX](t: FunctionLeaf[AX, CX]): Boolean =
    t.definition == this
}

sealed class FunctionLeaf[A, C](val function: C => A, override val definition: FunctionLeafDefinition[A, C]) extends Leaf[A, C] {
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

sealed abstract class BranchDefinition[A: ClassTag, C, P](
  name: String,
  repository: Repository[C]
) extends Definition[A, C](name, repository) {
  override type TREE <: Branch[A, C, P]
  override type CompatibleType <: BranchDefinition[A, C, P]

  def create(children: Seq[Tree[_, C]]): TREE

  override def toString() =
    s"(${name} ${childClasses.map(_.runtimeClass.getName).mkString(" ")}) => ${klass.runtimeClass.getName}"
}

sealed abstract class Branch[A, C, P] extends Tree[A, C] {
  def children: Seq[Tree[_, C]]
  protected def apply0(ctx: C): A

  override lazy val isConstant = children.forall(_.isConstant)

  override def definition: BranchDefinition[A, C, P]
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

sealed class Branch2Definition[A: ClassTag, C, B1: ClassTag, B2: ClassTag](
  name: String,
  repository: Repository[C],
  function: (C, Tree[B1, C], Tree[B2, C]) => A
) extends BranchDefinition[A, C, (Tree[B1, C], Tree[B2, C])](name, repository) {
  override type TREE = Branch2[A, C, B1, B2]
  override type CompatibleType <: Branch2Definition[A, C, B1, B2]

  override val childClasses = Seq(implicitly[ClassTag[B1]], implicitly[ClassTag[B2]])
  override def create(children: Seq[Tree[_, C]]): Branch2[A, C, B1, B2] = {
    require(children.size == 2)
    require(children.zip(childClasses).forall { case(c, k) => klass.runtimeClass.isAssignableFrom(c.definition.klass.runtimeClass) })
    new Branch2(
      this,
      children(0).asInstanceOf[Tree[B1, C]],
      children(1).asInstanceOf[Tree[B2, C]],
      function
    )
  }
  override def randomTree(depth: Int)(implicit random: Random): Branch2[A, C, B1, B2] = {
    create(Seq(
      repository.randomTree[B1](depth - 1),
      repository.randomTree[B2](depth - 1)
    ))
  }
  def unapply(t: Branch2[_, _, _, _]): Option[(Tree[B1, C], Tree[B2, C])] =
    t match {
      case b: Branch2[A, C, B1, B2] if b.definition == this =>
        val x: Branch2[A, C, B1, B2] = b // I dont known why but it need
        Some(x.child1 -> x.child2)
      case _ =>
        None
    }
}

sealed class Branch2[A, C, B1, B2](
  override val definition: BranchDefinition[A, C, (Tree[B1, C], Tree[B2, C])],
  val child1: Tree[B1, C],
  val child2: Tree[B2, C],
  val function: (C, Tree[B1, C], Tree[B2, C]) => A
) extends Branch[A, C, (Tree[B1, C], Tree[B2, C])] {
  override val children = Seq(child1, child2)
  override def apply0(ctx: C): A =
    function(ctx, child1, child2)
}
