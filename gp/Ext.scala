package com.todesking.scalagp

import scala.util.Random

object Ext {
  implicit class SeqExt[A](self: Seq[A]) {
    def weightedSampleBy(f: A => Int)(implicit random: Random): Option[A] = {
      val withWeight = self.zip(self.map(f(_)))
      val sum = withWeight.map(_._2).sum
      val th = random.nextInt(sum)
      var weight = 0
      withWeight.foreach { case(elm, w) =>
        weight += w
        if(th < weight) return Some(elm)
      }
      None
    }
    def sample()(implicit random: Random): Option[A] =
      if(self.isEmpty) None
      else Some(self(random.nextInt(self.size)))
    def sample(size: Int)(implicit random: Random): Traversable[A] = {
      require(size <= self.size)
      self.zip((0 to self.size).map { _ => random.nextDouble }).sortBy(_._2).slice(0 ,size).map(_._1)
    }
  }
}
