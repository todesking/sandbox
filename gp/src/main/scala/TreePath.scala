package com.todesking.scalagp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait TreePath[A, C, V] {
  type RootType = A
  type ContextType = C
  type ValueType = V

  def root: Tree[A, C]
  def value: Tree[V, C]
  def klass: Class[_ <: V] = value.definition.klass
  def replace(tree: Tree[V, C]): Tree[A, C]
  def unsafeReplace(tree: Tree[_, C]): Tree[A, C] =
    replace(tree.asInstanceOf[Tree[V, C]])
}
object TreePath {
  case class Root[A, C](override val root: Tree[A, C]) extends TreePath[A, C, A] {
    override def value: Tree[A, C] = root
    override def replace(tree: Tree[A, C]): Tree[A, C] = tree
  }
  case class Descent[A, C, V, PV](parent: TreePath[A, C, PV], index: Int, override val value: Tree[V, C]) extends TreePath[A, C, V] {
    override def root = parent.root
    override def replace(tree: Tree[V, C]): Tree[A, C] = {
      parent.value match {
        case b: Branch[PV, C, _] =>
          parent.replace(b.replaceChild(index, tree))
        case l: Leaf[_, C] =>
          throw new AssertionError()
      }
    }
  }
}

