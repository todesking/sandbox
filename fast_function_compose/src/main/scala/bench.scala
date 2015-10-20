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

object VeryFastCompose {
  def buildCompose[X, Y, Z](f1: X => Y, f2: Y => Z): VeryFastComposed[X, Z] = {
    val klass = generateComposerFor(f1, f2)
    klass.getConstructor(classOf[Function1[_, _]], classOf[Function1[_, _]])
      .newInstance(f1, f2)
      .asInstanceOf[VeryFastComposed[X, Z]]
  }

  private[this] var nextClassId = 0
  private[this] def getNextClassId(): Int = synchronized {
    nextClassId += 1
    nextClassId
  }

  private[this] def generateComposerFor(f: Function1[_, _], g: Function1[_, _]): Class[_] = {
    import javassist.{ ClassPool, ClassClassPath }
    val pool = ClassPool.getDefault()
    pool.appendClassPath(new ClassClassPath(classOf[VeryFastComposedTemplate[_, _, _]]))
    val baseName = classOf[VeryFastComposedTemplate[_, _, _]].getName

    val notSpecialized = { x: Any => x }

    def specialized(f: Function1[_, _], sig: String): Boolean = {
      require(sig.length == 2)
      val name = s"apply$$mc${sig}$$sp"
      val klass = f.getClass
      val specializedApplies = klass.getDeclaredMethods().filter(_.getName.startsWith("apply$mc"))
      (specializedApplies.size == 1 && specializedApplies.head.getName == name) || {
        classOf[VeryFastComposed[_, _]].isAssignableFrom(klass) &&
          s"^VeryFastComposed${sig(1)}.${sig(0)}[0-9]+$$".r.unapplySeq(klass.getSimpleName).nonEmpty
      }
    }

    def specializedSig(f: Function1[_, _]): String = {
      if (specialized(f, "II")) "II"
      else if (specialized(f, "DI")) "ID"
      else if (specialized(f, "ID")) "DI"
      else if (specialized(f, "DD")) "DD"
      else ""
    }

    val sig =
      (specializedSig(f), specializedSig(g)) match {
        case ("II", "II") => "III"
        case ("II", "ID") => "IID"
        case ("ID", "DI") => "IDI"
        case ("ID", "DD") => "IDD"
        case ("DI", "II") => "DII"
        case ("DI", "ID") => "DID"
        case ("DD", "DI") => "DII"
        case ("DD", "DD") => "DDD"
        case x => ""
      }
    // return Class.forName("com.todesking.sandbox.VeryFastComposedTemplate" + sig, true, getClass.getClassLoader)

    val template = pool.get(baseName + sig)
    template.setName(s"VeryFastComposed${sig}${getNextClassId()}")
    template.toClass()
  }
}

trait VeryFastComposed[A, B] extends Function1[A, B] {
}

class VeryFastComposedTemplate[A, B, C](private[this] val f: A => B, private[this] val g: B => C) extends VeryFastComposed[A, C] {
  override final def apply(a: A): C = g(f(a))
}

class VeryFastComposedTemplateIII(private[this] val f: Int => Int, private[this] val g: Int => Int) extends VeryFastComposed[Int, Int] {
  override final def apply(a: Int): Int = g(f(a))
}

class VeryFastComposedTemplateIID(private[this] val f: Int => Int, private[this] val g: Int => Double) extends VeryFastComposed[Int, Double] {
  override final def apply(a: Int): Double = g(f(a))
}

class VeryFastComposedTemplateIDI(private[this] val f: Int => Double, private[this] val g: Double => Int) extends VeryFastComposed[Int, Int] {
  override final def apply(a: Int): Int = g(f(a))
}

class VeryFastComposedTemplateIDD(private[this] val f: Int => Double, private[this] val g: Double => Double) extends VeryFastComposed[Int, Double] {
  override final def apply(a: Int): Double = g(f(a))
}

class VeryFastComposedTemplateDII(private[this] val f: Double => Int, private[this] val g: Int => Int) extends VeryFastComposed[Double, Int] {
  override final def apply(a: Double): Int = g(f(a))
}

class VeryFastComposedTemplateDID(private[this] val f: Double => Int, private[this] val g: Int => Double) extends VeryFastComposed[Double, Double] {
  override final def apply(a: Double): Double = g(f(a))
}

class VeryFastComposedTemplateDDI(private[this] val f: Double => Double, private[this] val g: Double => Int) extends VeryFastComposed[Double, Int] {
  override final def apply(a: Double): Int = g(f(a))
}

class VeryFastComposedTemplateDDD(private[this] val f: Double => Double, private[this] val g: Double => Double) extends VeryFastComposed[Double, Double] {
  override final def apply(a: Double): Double = g(f(a))
}

case class VeryFastCompose[A, B, C](f: A => B, g: B => C) extends Function1[A, C] {
  override def apply(a: A): C = proc(a)

  protected lazy val proc = build()

  private[this] def build(): A => C = (f match {
    case vf: VeryFastCompose[A, _, _] =>
      VeryFastCompose.buildCompose(vf.proc, g)
    case f =>
      VeryFastCompose.buildCompose(f, g)
  })

  override def andThen[D](h: C => D): VeryFastCompose[A, _, D] = h match {
    case VeryFastCompose(h1, h2) =>
      andThen(h1).andThen(h2)
    case h1 =>
      VeryFastCompose(proc, h1)
  }
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

  val baseline = {
    def F(x: Int) = f8(f7(f6(f5(f4(f3(f2(f1(x))))))))
    x: Int => F(F(F(F(x))))
  }

  val standardF = {
    def F = f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }

  val fastF = {
    def F = FastCompose(f1) andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }

  val veryFastF = {
    def F = VeryFastCompose(f1, f2) andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }

}

object Main {
  def main(args: Array[String]): Unit = {
    (0 until 10) foreach { i =>
      println(Seq(Bench.standardF(i), Bench.fastF(i), Bench.veryFastF(i)).mkString(", "))
    }
  }
}

/*
[info] # VM version: JDK 1.8.0_60, VM 25.60-b23
[info] Benchmark        Mode  Cnt      Score     Error  Units
[info] Bench.baseline  thrpt  200  21301.948 ± 240.846  ops/s
[info] Bench.fast      thrpt  200   5897.393 ± 139.765  ops/s
[info] Bench.standard  thrpt  200   4246.495 ±  80.637  ops/s
[info] Bench.veryFast  thrpt  200  12910.577 ± 227.401  ops/s
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

  @Benchmark
  def veryFast(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.veryFastF(i) }
    x
  }
}
