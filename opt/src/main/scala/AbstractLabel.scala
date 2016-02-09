package com.todesking.hoge

import scala.collection.mutable

abstract class AbstractLabel() extends AnyRef {
  val innerId = AbstractLabel.nextId()
  override def equals(other: Any): Boolean =
    other match {
      case l: AbstractLabel => this.innerId == l.innerId
      case _ => false
    }
  override def hashCode: Int =
    innerId.hashCode

  override def toString = s"${getClass.getName}#${innerId}"
}
object AbstractLabel {
  private[this] var _nextId = 0
  private def nextId(): Int = synchronized {
    val n = _nextId
    _nextId = _nextId + 1
    n
  }
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
  class Merger[P, L <: AbstractLabel](fresh: => L) {
    private[this] val merges = new mutable.HashMap[(P, L), mutable.Set[L]] with mutable.MultiMap[(P, L), L]
    private[this] val cache = new mutable.HashMap[(P, L, L), L]

    def toMap: Map[(P, L), Set[L]] = merges.mapValues(_.toSet).toMap

    // TODO: Is this really enough?
    def merge(pos: P, l1: L, l2: L): L =
      cache.get((pos, l1, l2)) getOrElse {
        val m = merge0(pos, l1, l2)
        cache((pos, l1, l2)) = m
        cache((pos, l2, l1)) = m
        m
      }

    private[this] def merge0(pos: P, l1: L, l2: L): L =
      if(l1 == l2) {
        l1
      } else if(merges.contains(pos -> l1)) {
        if(merges.contains(pos -> l2)) throw new AssertionError
        merges.addBinding(pos -> l1, l2)
        println(s"add ${l2} to ${l1} @ ${pos}")
        l1
      } else if(merges.contains(pos -> l2)) {
        merges.addBinding(pos -> l2, l1)
        println(s"add ${l1} to ${l1} @ ${pos}")
        l2
      } else if(merges.find(_._1._2 == l1).map(_._2.contains(l2)) getOrElse false) {
        l1
      } else if(merges.find(_._1._2 == l2).map(_._2.contains(l1)) getOrElse false) {
        l2
      } else {
        val m = fresh
        merges.addBinding(pos -> m, l1)
        merges.addBinding(pos -> m, l2)
        println(s"merge ${l2}, ${l1} to ${m} @ ${pos}")
        m
      }
  }

  trait NamerProvider[A <: AbstractLabel] {
    def namer(idPrefix: String, namePrefix: String): Namer[A] = new Namer(idPrefix, namePrefix)
  }
  trait AssignerProvider[A <: AbstractLabel] { self: { def fresh(): A } =>
    def assigner[B](): Assigner[B, A] = new Assigner(fresh())
  }
}

