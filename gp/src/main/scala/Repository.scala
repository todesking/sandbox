package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.reflect.ClassTag

import Ext._

class Repository[Ctx] {
  private[this] var _definitions =
    new ArrayBuffer[Definition[_, Ctx]]

  private[this] def register[A <: Definition[_, Ctx]](definition: A): A  = {
    if(definitionByName(definition.name).nonEmpty)
      throw new IllegalArgumentException(s""""${definition.name}" is already defined""")
    _definitions += definition
    definition
  }

  def registerConstLeaf[A: ClassTag]
      (name: String, generateValue: () => A)
      : ConstLeafDefinition[A, Ctx] =
    register(new ConstLeafDefinition[A, Ctx](name, this, generateValue))

  def registerFunctionLeaf[A: ClassTag](name: String)(f: Ctx => A): FunctionLeafDefinition[A, Ctx] =
    register(new FunctionLeafDefinition[A, Ctx](name, this, f))

  def registerBranch2[A: ClassTag, B1: ClassTag, B2: ClassTag]
      (name: String)
      (f: (Ctx, Tree[B1, Ctx], Tree[B2, Ctx]) => A)
      : Branch2Definition[A, Ctx, B1, B2] =
    register(new Branch2Definition[A, Ctx, B1, B2](name, this, f))

  def registerOptimizerNode[A: ClassTag, D](name: String)(f: (Ctx, D) => A): OptimizeDefinition[A, Ctx, D] =
    new OptimizeDefinition[A, Ctx, D](name, f, this)

  private[this] val optimizeRules = new ArrayBuffer[OptimizeRule[Ctx]]

  def registerOptimizer[A: ClassTag, D](name: String)(ruleDef: PartialFunction[Tree[_, Ctx], Tree[A, Ctx] => OptimizedTree[A, Ctx, D]]): OptimizeRule[Ctx] = {
    val rule = new OptimizeRule[Ctx](name, ruleDef.lift.asInstanceOf[Tree[_, Ctx] => Option[Tree[_, Ctx] => OptimizedTree[_, Ctx, _]]])
    optimizeRules += rule
    rule
  }

  def disableOptimizer(rule: OptimizeRule[Ctx]): Unit = {
    optimizeRules -= rule
  }

  def optimizerEnabled(rule: OptimizeRule[Ctx]): Boolean =
    optimizeRules.contains(rule)

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

  def allDefinitions: Traversable[Definition[_, Ctx]] =
    _definitions

  def definitions[A: ClassTag]: Traversable[Definition[A, Ctx]] =
    _definitions.filter { d =>
      implicitly[ClassTag[A]].runtimeClass.isAssignableFrom(d.klass.runtimeClass)
    }.asInstanceOf[Traversable[Definition[A, Ctx]]]

  def leafDefinitions[A: ClassTag]: Traversable[LeafDefinition[A, Ctx]] =
    _definitions
      .filter(_.isInstanceOf[LeafDefinition[_, _]])
      .asInstanceOf[Traversable[LeafDefinition[A, Ctx]]]

  def randomTree[A: ClassTag](depth: Int)(implicit random: Random): Tree[A, Ctx] =
    if(depth == 0) {
      leafDefinitions[A].toSeq.sample().get.create()
    } else {
      definitions[A].toSeq.sample().getOrElse(classNotRegistered[A]).randomTree(depth - 1)
    }

  def definitionByName(name: String): Option[Definition[_, Ctx]] =
    allDefinitions.filter(_.name == name).headOption

  def parse[A: ClassTag](s: String): Tree[A, Ctx] =
    new Parser[A, Ctx](this).parse(implicitly[ClassTag[A]], s)

  private[this] def classNotRegistered[A: ClassTag] =
    throw new IllegalStateException(s"Tree definition for ${implicitly[ClassTag[A]].runtimeClass.getName} is not registered")
  override def equals(other: Any) = other match {
    case r: Repository[Ctx] => this eq r
    case _ => false
  }
  override def hashCode = super.hashCode
}
