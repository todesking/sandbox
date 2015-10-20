package com.todesking.sandbox

import org.openjdk.jmh.annotations.{ Benchmark, State }

sealed trait FastCompose[A, B] extends Function[A, B] {
  private[this] lazy val proc = build()
  protected def build(): A => B
  protected def andThen0[C](f: B => C): FastCompose[A, C]

  // TODO: compose

  override def apply(a: A): B = proc(a)

  override def andThen[C](f: B => C): FastCompose[A, C] = f match {
    case FastCompose1(a1) => andThen(a1)
    case FastCompose2(a1, a2) => andThen(a1).andThen(a2)
    case FastCompose3(a1, a2, a3) => andThen(a1).andThen(a2).andThen(a3)
    case FastCompose4(a1, a2, a3, a4) => andThen(a1).andThen(a2).andThen(a3).andThen(a4)
    case FastCompose5(a1, a2, a3, a4, a5) => andThen(a1).andThen(a2).andThen(a3).andThen(a4).andThen(a5)
    case a1 => andThen0(a1)
  }
}

object FastCompose {
  def apply[A, B](f: A => B): FastCompose[A, B] = f match {
    case fc: FastCompose[A, B] => f.asInstanceOf[FastCompose[A, B]]
    case f => FastCompose1(f)
  }
}

final case class FastCompose1[A, B](f: A => B) extends FastCompose[A, B] {
  override def build() = f
  override def andThen0[X](x: B => X): FastCompose[A, X] = FastCompose2(f, x)
  override def toString = "[fc]"
}

final case class FastCompose2[A, B, C](f: A => B, g: B => C) extends FastCompose[A, C] {
  override def build() = { x: A => g(f(x)) }
  override def andThen0[X](x: C => X): FastCompose[A, X] = FastCompose3(f, g, x)
  override def toString = s"(${f}, ${g})"
}

final case class FastCompose3[A, B, C, D](f: A => B, g: B => C, h: C => D) extends FastCompose[A, D] {
  override def build() = { x: A => h(g(f(x))) }
  override def andThen0[X](x: D => X): FastCompose[A, X] = FastCompose4(f, g, h, x)
  override def toString = s"(${f}, ${g}, ${h})"
}

final case class FastCompose4[A, B, C, D, E](f: A => B, g: B => C, h: C => D, i: D => E) extends FastCompose[A, E] {
  override def build() = { x: A => i(h(g(f(x)))) }
  override def andThen0[X](x: E => X): FastCompose[A, X] = FastCompose5(f, g, h, i, x)
  override def toString = s"(${f}, ${g}, ${h})"
}

final case class FastCompose5[A, B, C, D, E, F](f: A => B, g: B => C, h: C => D, i: D => E, j: E => F) extends FastCompose[A, F] {
  override def build() = { x: A => j(i(h(g(f(x))))) }
  override def andThen0[X](x: F => X): FastCompose[A, X] = FastCompose1(this).andThen(x) // FastCompose6(f, g, h, i, j, x)
  override def toString = s"(${f}, ${g}, ${h}, ${i})"
}

object Bench {
  val f1: Int => Int = { x => x + 1 }
  val f2: Int => Double = { x => x + 10.0 }
  val f3: Double => Int = { x => (x * 100).toInt }
  val f4: Int => Double = { x => x + 1.5 }
  val f5: Double => Double = { x => x * 0.01 }
  val f6: Double => Double = { x => x - 200.0 }
  val f7: Double => Int = { x => x.toInt }
  val f8: Int => Int = { x => x + 10 }

  val standardF = {
    def F = f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }
  val fastF = {
    def F = FastCompose(f1) andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }

  val baseline = {
    def F(x: Int) = f8(f7(f6(f5(f4(f3(f2(f1(x))))))))
    x: Int => F(F(F(F(x))))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println(Bench.fastF)
    (0 until 10) foreach { i =>
      println(Bench.standardF(i) -> Bench.fastF(i))
    }
  }
}

/*
[info] # VM version: JDK 1.8.0_60, VM 25.60-b23
[info] Benchmark        Mode  Cnt      Score      Error  Units
[info] Bench.baseline  thrpt   10  19838.660 ± 2798.280  ops/s
[info] Bench.fast      thrpt   10   5143.457 ±  644.086  ops/s
[info] Bench.standard  thrpt   10   4166.748 ±  489.675  ops/s
*/

class Bench {
  @Benchmark
  def baseline(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.baseline(i) }
    x
  }

  @Benchmark
  def standard(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.standardF(i) }
    x
  }

  @Benchmark
  def fast(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.fastF(i) }
    x
  }
}
