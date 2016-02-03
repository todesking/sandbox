package com.todesking.hoge

import scala.collection.mutable

abstract class AbstractLabel() extends AnyRef {
  override def equals(other: Any): Boolean =
    other match {
      case r: AnyRef => this eq r
      case _ => false
    }
  override def hashCode: Int =
    System.identityHashCode(this)
}
object AbstractLabel {
  class Namer[A <: AbstractLabel](idPrefix: String, namePrefix: String) {
    private[this] val ids = mutable.HashMap.empty[A, Int]
    private[this] var nextId = 0

    def num(l: A): Int =
      ids.get(l) getOrElse {
        ids(l) = nextId
        nextId += 1
        ids(l)
      }
    def id(l: A): String = s"${idPrefix}${num(l)}"
    def name(l: A): String = s"${namePrefix}${num(l)}"
  }
  class Assigner[A, L <: AbstractLabel](fresh: => L) {
    private[this] val mapping = mutable.HashMap.empty[A, L]
    def apply(key: A): L =
      mapping.get(key) getOrElse {
        val l = fresh
        mapping(key) = l
        l
      }
  }

  trait NamerProvider[A <: AbstractLabel] {
    def namer(idPrefix: String, namePrefix: String): Namer[A] = new Namer(idPrefix, namePrefix)
  }
  trait AssignerProvider[A <: AbstractLabel] { self: { def fresh(): A } =>
    def assigner[B](): Assigner[B, A] = new Assigner(fresh())
  }
}

