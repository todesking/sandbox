package com.todesking.scalagp

import scala.util.Random
import scala.reflect.ClassTag

import Ext._

trait Computable[A, C] {
  def apply(ctx: C): A
}

sealed abstract class Definition[A: ClassTag, C](val name: String, val repository: Repository[C]) extends Equals {
  type TREE <: Tree[A, C]
  type CompatibleType <: Definition[A, C]

  val klass: ClassTag[A] = implicitly[ClassTag[A]]
  def arity: Int = childClasses.size
  def childClasses: Seq[ClassTag[_]]
  def create(children: Seq[Tree[_, C]]): TREE
  def compatibleShapeDefinitions(): Seq[CompatibleType] =
    repository.definitions(klass).filter(isCompatible(_)).map(_.asInstanceOf[CompatibleType]).toSeq
  def isCompatible(d: Definition[_, C]): Boolean =
    d.klass == klass && d.arity == arity && d.childClasses.zip(childClasses).forall { case (a, b) => a.runtimeClass.isAssignableFrom(b.runtimeClass) }
  override def canEqual(rhs: Any): Boolean =
    rhs.isInstanceOf[Definition[_, _]]
  override def equals(rhs: Any): Boolean = rhs match {
    case d: Definition[A, C] =>
      d.canEqual(this) && d.repository == this.repository && d.name == this.name
    case _ => false
  }
}

sealed abstract class Tree[A, C] extends Computable[A, C] with Equals {
  override def apply(ctx: C): A

  def definition: Definition[A, C]
  def klass: ClassTag[A] = definition.klass

  def height: Int
  def size: Int

  def ensureKlass[AX: ClassTag]: Tree[AX, C] =
    if(klass == implicitly[ClassTag[AX]]) this.asInstanceOf[Tree[AX, C]]
    else throw new AssertionError()

  def isConstant: Boolean = false
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
  def makeOptimized(optimized: Computable[A, C]): OptimizedTree[A, C] =
    optimized match {
      case OptimizedTree(w, o) =>
        new OptimizedTree(OptimizedTree.unwrap(this), o)
      case o =>
        new OptimizedTree(OptimizedTree.unwrap(this), o)
    }

  override def canEqual(rhs: Any): Boolean =
    rhs.isInstanceOf[Tree[_, _]]
}

sealed case class OptimizedTree[A, C](
  val wrapped: Tree[A, C],
  val optimized: Computable[A, C]
) extends Tree[A, C] {
  override def apply(ctx: C) = optimized.apply(ctx)
  override def definition = wrapped.definition
  override def height = wrapped.size
  override def size = wrapped.size
  override def allPaths[R](base: TreePath[R, C, A]) =
    wrapped.allPaths(base)
  override def toString =
    s"[${optimized.toString}]${wrapped.toString}"
}

object OptimizedTree {
  def nakedTree[A, C](tree: Tree[A, C]): Tree[A, C] =
    tree match {
      case OptimizedTree(_, o) =>
        o match {
          case t: Tree[A, C] =>
            t
          case _ =>
            tree
        }
      case t => t
    }
  def unwrap[A, C](tree: Tree[A, C]): Tree[A, C] =
    tree match {
      case b: Branch[A, C] =>
        b.definition.create(
          b.children.map(unwrap(_))
        )
      case OptimizedTree(w, o) =>
        w
      case t =>
        t
    }
}

sealed abstract class LeafDefinition[A: ClassTag, C](
  name: String,
  repository: Repository[C]
) extends Definition[A, C](name, repository) {
  override type TREE <: Leaf[A, C]
  override type CompatibleType = LeafDefinition[A, C]
  override val childClasses = Seq.empty
  override def create(children: Seq[Tree[_, C]]) = {
    require(children.isEmpty)
    create()
  }
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
  override def create() =
    create(generateValue())

  def apply(value: A) = create(value)
  def create(value: A) =
    new ConstLeaf[A, C](value, this)
  def unapply[AX, CX](tree: Tree[AX, CX]): Option[A] =
    tree match {
      case OptimizedTree(wrapped, optimized: Tree[AX, CX]) =>
        unapply(optimized)
      case leaf if leaf.definition == this =>
        Some(leaf.asInstanceOf[ConstLeaf[A, C]].value)
      case _ =>
        None
    }
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

  def unapply[AX, CX](tree: Tree[AX, CX]): Boolean =
    tree match {
      case OptimizedTree(w, o: TREE) =>
        unapply(o)
      case t if t.definition == this =>
        true
      case _ => false
    }
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

sealed abstract class BranchDefinition[A: ClassTag, C](
  name: String,
  repository: Repository[C]
) extends Definition[A, C](name, repository) {
  override type TREE <: Branch[A, C]
  override type CompatibleType <: BranchDefinition[A, C]

  override def toString() =
    s"(${name} ${childClasses.map(_.runtimeClass.getName).mkString(" ")}) => ${klass.runtimeClass.getName}"
}

sealed abstract class Branch[A, C] extends Tree[A, C] {
  def children: Seq[Tree[_, C]]
  protected def apply0(ctx: C): A

  override lazy val isConstant = children.forall(_.isConstant)

  override def definition: BranchDefinition[A, C]
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

  def replaceChild(index: Int, c: Tree[_, C]): Branch[A, C] =
    definition.create(children.updated(index, c))

  override def allPaths[R](base: TreePath[R, C, A]): Traversable[TreePath[R, C, _]] =
    base +: children.zipWithIndex.flatMap { case (c: Tree[_, C], i) => c.allPaths(TreePath.Descent(base, i, c)) }

  override def toString =
    s"(${definition.name} ${children.map(_.toString).mkString(" ")})"

  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: Branch[A, C] @unchecked => rhs.canEqual(this) && rhs.children == this.children
    case _ => false
  }

  override def hashCode =
    children.foldLeft(definition.hashCode) { (a, x) => a ^ x.hashCode }
}

sealed class Branch2Definition[A: ClassTag, C, B1: ClassTag, B2: ClassTag](
  name: String,
  repository: Repository[C],
  function: (C, Tree[B1, C], Tree[B2, C]) => A
) extends BranchDefinition[A, C](name, repository) {
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
  def apply(t1: Tree[B1, C], t2: Tree[B2, C]) =
    create(Seq(t1, t2))
  def unapply[AX, CX](t: Tree[AX, CX]): Option[(Tree[B1, C], Tree[B2, C])] =
    t match {
      case b: Branch2[AX, CX, B1, B2] if b.definition == this =>
        val x: Branch2[A, C, B1, B2] = b.asInstanceOf[Branch2[A, C, B1, B2]]
        Some(OptimizedTree.nakedTree(x.child1) -> OptimizedTree.nakedTree(x.child2))
      case OptimizedTree(w, o: TREE) =>
        unapply(o)
      case _ =>
        None
    }
}

sealed class Branch2[A, C, B1, B2](
  override val definition: BranchDefinition[A, C],
  val child1: Tree[B1, C],
  val child2: Tree[B2, C],
  val function: (C, Tree[B1, C], Tree[B2, C]) => A
) extends Branch[A, C] {
  override val children = Seq(child1, child2)
  override def apply0(ctx: C): A =
    function(ctx, child1, child2)
}

sealed class Branch3Definition[A: ClassTag, C, B1: ClassTag, B2: ClassTag, B3: ClassTag](
  name: String,
  repository: Repository[C],
  function: (C, Tree[B1, C], Tree[B2, C], Tree[B3, C]) => A
) extends BranchDefinition[A, C](name, repository) {
  override type TREE = Branch3[A, C, B1, B2, B3]
  override type CompatibleType <: Branch3Definition[A, C, B1, B2, B3]

  override val childClasses = Seq(implicitly[ClassTag[B1]], implicitly[ClassTag[B2]], implicitly[ClassTag[B3]])
  override def create(children: Seq[Tree[_, C]]): Branch3[A, C, B1, B2, B3] = {
    assert(children.size == arity)
    new Branch3(
      this,
      children(0).asInstanceOf[Tree[B1, C]],
      children(1).asInstanceOf[Tree[B2, C]],
      children(2).asInstanceOf[Tree[B3, C]],
      function
    )
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

sealed class Branch3[A, C, B1, B2, B3](
  override val definition: BranchDefinition[A, C],
  val child1: Tree[B1, C],
  val child2: Tree[B2, C],
  val child3: Tree[B3, C],
  val function: (C, Tree[B1, C], Tree[B2, C], Tree[B3, C]) => A
) extends Branch[A, C] {
  override val children = Seq(child1, child2, child3)
  override def apply0(ctx: C): A =
    function(ctx, child1, child2, child3)
}
