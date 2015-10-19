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

case class FastCompose1[A, B](f: A => B) extends FastCompose[A, B] {
  override def build() = f
  override def andThen0[X](x: B => X): FastCompose[A, X] = FastCompose2(f, x)
}

case class FastCompose2[A, B, C](f: A => B, g: B => C) extends FastCompose[A, C] {
  override def build() = f andThen g
  override def andThen0[X](x: C => X): FastCompose[A, X] = FastCompose3(f, g, x)
}

case class FastCompose3[A, B, C, D](f: A => B, g: B => C, h: C => D) extends FastCompose[A, D] {
  override def build() = { x: A => h(g(f(x))) }
  override def andThen0[X](x: D => X): FastCompose[A, X] = FastCompose4(f, g, h, x)
}

case class FastCompose4[A, B, C, D, E](f: A => B, g: B => C, h: C => D, i: D => E) extends FastCompose[A, E] {
  override def build() = { x: A => i(h(g(f(x)))) }
  override def andThen0[X](x: E => X): FastCompose[A, X] = FastCompose5(f, g, h, i, x)
}

case class FastCompose5[A, B, C, D, E, F](f: A => B, g: B => C, h: C => D, i: D => E, j: E => F) extends FastCompose[A, F] {
  override def build() = { x: A => j(i(h(g(f(x))))) }
  override def andThen0[X](x: F => X): FastCompose[A, X] = FastCompose6(f, g, h, i, j, x)
}

case class FastCompose6[A, B, C, D, E, F, G](f: A => B, g: B => C, h: C => D, i: D => E, j: E => F, k: F => G) extends FastCompose[A, G] {
  override def build() = { x: A => k(j(i(h(g(f(x)))))) }
  override def andThen0[X](x: G => X): FastCompose[A, X] = FastCompose7(f, g, h, i, j, k, x)
}

case class FastCompose7[A, B, C, D, E, F, G, H](f: A => B, g: B => C, h: C => D, i: D => E, j: E => F, k: F => G, l: G => H) extends FastCompose[A, H] {
  override def build() = { x: A => l(k(j(i(h(g(f(x))))))) }
  override def andThen0[X](x: H => X): FastCompose[A, X] = FastCompose8(f, g, h, i, j, k, l, x)
}

case class FastCompose8[A, B, C, D, E, F, G, H, I](f: A => B, g: B => C, h: C => D, i: D => E, j: E => F, k: F => G, l: G => H, m: H => I) extends FastCompose[A, I] {
  override def build() = { x: A => m(l(k(j(i(h(g(f(x)))))))) }
  override def andThen0[X](x: I => X): FastCompose[A, X] = FastCompose2(this, x)
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
    val F = f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }
  val fastF = {
    val F = FastCompose(f1) andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    (0 until 10) foreach { i =>
      println(Bench.standardF(i) -> Bench.fastF(i))
    }
  }
}

/*
[info] # VM version: JDK 1.8.0_60, VM 25.60-b23
[info] Benchmark        Mode  Cnt     Score     Error  Units
[info] Bench.fast      thrpt  200  9098.848 ± 568.496  ops/s
[info] Bench.standard  thrpt  200  4362.272 ±  58.180  ops/s
*/

class Bench {
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
