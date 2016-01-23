import scalaz.Arrow
import scalaz.syntax.arrow._
import scala.specialized
import com.todesking.fast_function_composer.FastComposable
import FastComposable.noHint
import com.todesking.arrow_builder.{ ArrowDelayLoop, Signal, ArrowBuilder }

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.existentials

sealed trait SignalFunction[-A, +B] {
  final def buildProc(): A => B =
    FastComposable.compile(buildProc0(), true)

  // unoptimized version
  def buildProc0(): A => B

  def merge[C, D, E](rhs: SignalFunction[C, D])(f: (B, D) => E): SignalFunction[(A, C), E] =
    (this *** rhs).mapsnd(f.tupled)

  def +[C, D](rhs: SignalFunction[C, D])(implicit num: Numeric[D], evB: B <:< D): SignalFunction[(A, C), D] =
    this.merge(rhs) { (d1, d2) => num.plus(evB(d1), d2) }

  def *[C, D](rhs: SignalFunction[C, D])(implicit num: Numeric[D], evB: B <:< D): SignalFunction[(A, C), D] =
    this.merge(rhs) { (d1, d2) => num.times(evB(d1), d2) }

  def name(n: String): SignalFunction[A, B] =
    NamedSF(n, this)
}

case class NamedSF[A, B](name: String, sf: SignalFunction[A, B]) extends SignalFunction[A, B] {
  override def buildProc0() = sf.buildProc0()
  override def toString() =
    s"[${name}]"
}

case class FunctionSF[A, B](f: A => B) extends SignalFunction[A, B] {
  override def buildProc0(): A => B = f
  override def toString =
    "[function]"
}

case class IdSF[A]() extends SignalFunction[A, A] {
  override def buildProc0(): A => A = identity
  override def toString =
    "[id]"
}

case class ParallelSF[@specialized(Double, Int) A, @specialized(Double, Int) B, @specialized(Double, Int) C, @specialized(Double, Int) D](
    first: SignalFunction[A, B],
    second: SignalFunction[C, D]
) extends SignalFunction[(A, C), (B, D)] {
  override def buildProc0(): ((A, C)) => (B, D) = {
    val f = first.buildProc()
    val s = second.buildProc()
    ac: (A, C) => (f(ac._1) -> s(ac._2))
  }

  override def toString =
    s"(${first}) *** (${second})"
}

case class ComposeSF[A, B, C](sfAB: SignalFunction[A, B], sfBC: SignalFunction[B, C]) extends SignalFunction[A, C] {
  override def buildProc0(): A => C =
    noHint(sfAB.buildProc0()) andThen sfBC.buildProc0()

  override def toString =
    s"(${sfAB}) >>> (${sfBC})"
}

case class CombineSF[A, B, C](sfAB: SignalFunction[A, B], sfAC: SignalFunction[A, C]) extends SignalFunction[A, (B, C)] {
  override def buildProc0(): A => (B, C) = {
    val f1 = sfAB.buildProc()
    val f2 = sfAC.buildProc()

    (a: A) => (f1(a) -> f2(a))
  }

  override def toString =
    s"(${sfAB}) &&& (${sfAC})"
}

case class StatefulSF[@specialized(Double) A, @specialized(Double) B, @specialized(Double) C](sf: SignalFunction[(A, C), (B, C)], init: C) extends SignalFunction[A, B] {
  override def buildProc0(): A => B = {
    val proc = sf.buildProc()
    var i = init

    a => {
      val (b, ii) = proc(a -> i)
      i = ii
      b
    }
  }
  override def toString =
    s"[stateful(${init})](${sf})"
}

case class ConstSF[@specialized(Int, Double) A](value: A) extends SignalFunction[Unit, A] {
  override def buildProc0(): Unit => A =
    _ => value
  override def toString =
    s"[const(${value}]"
}

object SignalFunction {
  def build[A, B](f: Signal[SignalFunction, A, A] => ArrowBuilder[SignalFunction, A, B]): SignalFunction[A, B] =
    ArrowBuilder.build[SignalFunction, A, B](f)

  def delayLoop[A, B, C](init: C)(f: Signal[SignalFunction, (A, C), (A, C)] => ArrowBuilder[SignalFunction, (A, C), (B, C)]): SignalFunction[A, B] =
    ArrowBuilder.delayLoop(init)(f)

  implicit def arrowInstance: ArrowDelayLoop[SignalFunction] = new ArrowDelayLoop[SignalFunction] {
    override def arr[C, D](f: C => D): SignalFunction[C, D] =
      FunctionSF(f)

    override def id[C]: SignalFunction[C, C] =
      IdSF[C]()

    override def first[C, D, E](sf: SignalFunction[C, D]): SignalFunction[(C, E), (D, E)] =
      ParallelSF[C, D, E, E](sf, IdSF[E]())

    override def second[C, D, E](sf: SignalFunction[C, D]): SignalFunction[(E, C), (E, D)] =
      ParallelSF[E, E, C, D](IdSF(), sf)

    override def combine[A, B, C](fab: SignalFunction[A, B], fac: SignalFunction[A, C]): SignalFunction[A, (B, C)] =
      CombineSF(fab, fac)

    override def compose[C, D, E](sf1: SignalFunction[D, E], sf2: SignalFunction[C, D]): SignalFunction[C, E] =
      ComposeSF(sf2, sf1)

    override def delayLoop[A, B, @specialized(Double) C](init: C, a: SignalFunction[(A, C), (B, C)]): SignalFunction[A, B] =
      StatefulSF(a, init)

  }

  implicit def autoConst[A](a: A): SignalFunction[Unit, A] =
    const(a)

  def apply[A, B](f: A => B): SignalFunction[A, B] =
    FunctionSF[A, B](f)

  def stateful[A, B, @specialized(Int, Long, Double) C](init: C)(f: (A, C) => (B, C)): StatefulSF[A, B, C] =
    StatefulSF(FunctionSF[(A, C), (B, C)](_ match { case (a, c) => f(a, c) }), init)

  def stateOut[@specialized(Double) A, @specialized(Int, Long, Double) B](init: B)(f: (A, B) => B): StatefulSF[A, B, B] =
    stateful[A, B, B](init) {
      case (a, b) =>
        val b_ = f(a, b)
        (b_ -> b_)
    }

  def id[A]: SignalFunction[A, A] = IdSF[A]

  val unit: ConstSF[Unit] = ConstSF(())

  def const[@specialized(Int, Double) A](value: A): ConstSF[A] = ConstSF(value)

  def ignore[A]: SignalFunction[A, Unit] =
    SignalFunction[A, Unit] { _ => () }.name("ignore")

  def delay[@specialized(Int, Double) A](init: A): SignalFunction[A, A] =
    stateOut[A, A](init) { case (a, s) => a }

  def optimize[A, B](sf: SignalFunction[A, B]): SignalFunction[A, B] =
    sf match {
      case NamedSF(name, sf1) =>
        optimize(sf1)
      case ComposeSF(sf1, sf2) =>
        optimizeCompose(optimize(sf1), optimize(sf2))
      case ParallelSF(sf1, sf2) =>
        val o1 = optimize(blackMagic(sf1))
        val o2 = optimize(blackMagic(sf2))
        (o1, o2) match {
          case (IdSF(), IdSF()) => IdSF()
          case _ => ParallelSF(o1, o2).asInstanceOf[SignalFunction[A, B]]
        }
      case CombineSF(sf1, sf2) =>
        val o1 = optimize(blackMagic(sf1))
        val o2 = optimize(blackMagic(sf2))
        CombineSF(o1, o2)
      case StatefulSF(sf1, init) =>
        StatefulSF(optimize(sf1), init)
      case sf => sf
    }

  def optimizeCompose[A, B, C](sf1: SignalFunction[A, B], sf2: SignalFunction[B, C]): SignalFunction[A, C] =
    (sf1, sf2) match {
      case (IdSF(), s) => s.asInstanceOf[SignalFunction[A, C]]
      case (s, IdSF()) => s.asInstanceOf[SignalFunction[A, C]]
      case (ParallelSF(f1, s1), ParallelSF(f2, s2)) =>
        ParallelSF(
          optimizeCompose(blackMagic(f1), blackMagic(f2)),
          optimizeCompose(blackMagic(s1), blackMagic(s2))
        ).asInstanceOf[SignalFunction[A, C]]
      case (ComposeSF(f1, f2), f3) =>
        ComposeSF(f1, optimizeCompose(f2, f3))
      case (f1, ComposeSF(f2, f3)) =>
        ComposeSF(optimizeCompose(f1, f2), f3)
      case (CombineSF(f1, f2), ParallelSF(f3, f4)) =>
        CombineSF(optimizeCompose(blackMagic(f1), blackMagic(f3)), optimizeCompose(blackMagic(f2), blackMagic(f4)))
      case _ =>
        ComposeSF[A, B, C](sf1, sf2)
    }

  private[this] def blackMagic[A, B](sf: SignalFunction[_, _]): SignalFunction[A, B] =
    sf.asInstanceOf[SignalFunction[A, B]]
}

