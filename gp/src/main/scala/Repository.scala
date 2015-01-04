package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import Ext._

class Repository[Ctx] {
  private[this] var _definitions =
    new ArrayBuffer[Definition[_, Ctx, Tree[_, Ctx]]]

  private[this] def register[A <: Definition[_, Ctx, Tree[_, Ctx]]](definition: A): A  = {
    if(definitionByName(definition.name).nonEmpty)
      throw new IllegalArgumentException(s""""${definition.name}" is already defined""")
    _definitions += definition
    definition
  }

  def registerConstLeaf[A: Class]
      (name: String, generateValue: () => A)
      : Definition[A, Ctx, ConstLeaf[A, Ctx]] =
    register(
      new ConstLeafDefinition[A, Ctx, ConstLeaf[A, Ctx]](name, implicitly[Class[A]], this) {
        override def create(): ConstLeaf[A, Ctx] =
          new ConstLeaf(generateValue(), this)
        override def create(value: A): ConstLeaf[A, Ctx] =
          new ConstLeaf(value, this)
      }
    )

  def registerLeaf[A: Class](name: String)(f: Ctx => A): Definition[A, Ctx, FunctionLeaf[A, Ctx]] =
    register(
      new LeafDefinition[A, Ctx, FunctionLeaf[A, Ctx]](name, implicitly[Class[A]], this) {
        override def create() =
          new FunctionLeaf(f, this)
      }
    )

  def registerBranch2[A: Class, B1, B2]
      (name: String)
      (f: (Ctx, Tree[B1, Ctx], Tree[B2, Ctx]) => A)
      (implicit c1: Class[B1], c2: Class[B2])
      : Branch2Definition[A, Ctx, B1, B2, Branch2[A, Ctx, B1, B2]] =
    register(
      new Branch2Definition[A, Ctx, B1, B2, Branch2[A, Ctx, B1, B2]](name, implicitly[Class[A]], this) {
        override val childClasses = Seq(c1, c2)
        override def create(children: Seq[Tree[_, Ctx]]): Branch2[A, Ctx, B1, B2] = {
          require(children.size == 2)
          require(children.zip(childClasses).forall { case(c, k) => k.isAssignableFrom(c.definition.klass) })
          new Branch2(
            this,
            children(0).asInstanceOf[Tree[B1, Ctx]],
            children(1).asInstanceOf[Tree[B2, Ctx]],
            f
          )
        }
        override def randomTree(repository: Repository[Ctx], depth: Int)(implicit random: Random): Branch2[A, Ctx, B1, B2] = {
          create(Seq(
            repository.randomTree[B1](depth - 1),
            repository.randomTree[B2](depth - 1)
          ))
        }
      }
    )
  def registerOptimized[A: Class, D](name: String)(f: (Ctx, D) => A): OptimizeDefinition[A, Ctx, D] =
    new OptimizeDefinition[A, Ctx, D](name, implicitly[Class[A]], f, this)

  private[this] val optimizeRules = new ArrayBuffer[Tree[_, Ctx] => Option[Tree[_, Ctx] => OptimizedTree[_, Ctx, _]]]

  def optimizeRule[A: Class, D](ruleDef: PartialFunction[Tree[_, Ctx], Tree[A, Ctx] => OptimizedTree[A, Ctx, D]]): Unit =
    optimizeRules += ruleDef.lift.asInstanceOf[Tree[_, Ctx] => Option[Tree[_, Ctx] => OptimizedTree[_, Ctx, _]]]

  def optimize[A](tree: Tree[A, Ctx]): Tree[A, Ctx] =
    tree match {
      case t: Leaf[A, Ctx] =>
        optimize1(t)
      case t: Branch[A, Ctx, _] =>
        val children = t.children.map(optimize(_))
        if(t.children.zip(children).forall { case(c, oc) => c eq oc })
          optimize1(t)
        else
          optimize1(t.definition.create(children))
      case t =>
        t
    }

  def optimize1[A](tree: Tree[A, Ctx]): Tree[A, Ctx] =
    optimizeRules.foldLeft[Tree[A, Ctx]](tree) { (tree, rule) => rule(tree) map(_(unoptimize(tree)).asInstanceOf[Tree[A, Ctx]]) getOrElse tree }

  def unoptimize[A](tree: Tree[A, Ctx]): Tree[A, Ctx] =
    tree match {
      case t: Leaf[A, Ctx] =>
        t
      case t: Branch[A, Ctx, _] =>
        val children = t.children.map(unoptimize(_))
        if(t.children.zip(children).forall { case(c, oc) => c eq oc })
          t
        else
          t.definition.create(children)
      case t: OptimizedTree[A, Ctx, _] =>
        t.wrapped
    }

  def allDefinitions: Traversable[Definition[_, Ctx, Tree[_, Ctx]]] =
    _definitions

  def definitions[A: Class]: Traversable[Definition[A, Ctx, Tree[A, Ctx]]] =
    _definitions.filter { d =>
      implicitly[Class[A]].isAssignableFrom(d.klass)
    }.asInstanceOf[Traversable[Definition[A, Ctx, Tree[A, Ctx]]]]

  def leafDefinitions[A: Class]: Traversable[LeafDefinition[A, Ctx, _ <: Leaf[A, Ctx]]] =
    _definitions
      .filter(_.isInstanceOf[LeafDefinition[_, _, _]])
      .asInstanceOf[Traversable[LeafDefinition[A, Ctx, _ <: Leaf[A, Ctx]]]]

  def randomTree[A: Class](depth: Int)(implicit random: Random): Tree[A, Ctx] =
    if(depth == 0) {
      leafDefinitions[A].toSeq.sample().get.create()
    } else {
      definitions[A].toSeq.sample().getOrElse(classNotRegistered[A]).randomTree(this, depth)
    }

  def definitionByName(name: String): Option[Definition[_, Ctx, Tree[_, Ctx]]] =
    allDefinitions.filter(_.name == name).headOption

  def parse[A](s: String)(implicit klass: Class[A]): Tree[A, Ctx] =
    new Parser[A, Ctx](this).parse(klass, s)

  private[this] def classNotRegistered[A](implicit klass: Class[A]) =
    throw new IllegalStateException(s"Tree definition for ${implicitly[Class[A]].getName} is not registered")
  override def equals(other: Any) = other match {
    case r: Repository[Ctx] => this eq r
    case _ => false
  }
  override def hashCode = super.hashCode
}
