package com.todesking.scalagp

import scala.util.Random

object Ext {
  implicit class TraversableExt[A](self: Traversable[A]) {
    def weightedSampleBy(f: A => Int)(implicit random: Random): Option[A] = {
      if(self.isEmpty) {
        None
      } else {
        val withWeight = self.map { x => (x -> f(x)) }
        val sum = withWeight.map(_._2).sum
        val th = random.nextInt(sum)
        var weight = 0
        withWeight.foreach { case(elm, w) =>
          weight += w
          if(th < weight) return Some(elm)
        }
        throw new AssertionError()
      }
    }
  }
  implicit class SeqExt[A](self: Seq[A]) {
    def sample()(implicit random: Random): Option[A] =
      if(self.isEmpty) None
      else Some(self(random.nextInt(self.size)))
    def sample(size: Int)(implicit random: Random): Traversable[A] = {
      require(size <= self.size)
      self.zip(self.map{ _ => random.nextDouble() }).sortBy(_._2).slice(0 ,size).map(_._1)
    }
  }
}
