import scalaz.Arrow
import scalaz.syntax.arrow._
import scala.specialized
import com.todesking.fast_function_composer.FastComposable
import FastComposable.noHint

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

case class BypassSF[@specialized(Double, Int) A, @specialized(Double, Int) B, @specialized(Double, Int) C](sf: SignalFunction[A, B]) extends SignalFunction[(A, C), (B, C)] {
  override def buildProc0(): ((A, C)) => (B, C) = {
    val proc = sf.buildProc()
    ac: (A, C) => (proc(ac._1) -> ac._2)
  }

  override def toString =
    s"(${sf}) *** [bypass]"
}
case class ComposeSF[A, B, C](sfBC: SignalFunction[B, C], sfAB: SignalFunction[A, B]) extends SignalFunction[A, C] {
  override def buildProc0(): A => C =
    noHint(sfAB.buildProc0()) andThen sfBC.buildProc0()

  override def toString =
    s"(${sfAB}) >>> (${sfBC})"
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
    s"[consst(${value}]"
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
      BypassSF[C, D, E](sf)

    // TODO: second
    // TODO: combine

    override def compose[C, D, E](sf1: SignalFunction[D, E], sf2: SignalFunction[C, D]): SignalFunction[C, E] =
      (sf1, sf2) match {
        case (NamedSF(_, s1), s2) => compose(s1, s2)
        case (s1, NamedSF(_, s2)) => compose(s1, s2)
        case (IdSF(), s) => s.asInstanceOf[SignalFunction[C, E]]
        case (s, IdSF()) => s.asInstanceOf[SignalFunction[C, E]]
        case (BypassSF(sf1), BypassSF(sf2)) =>
          // Ugh
          BypassSF(
            compose(
              sf1.asInstanceOf[SignalFunction[Any, Any]],
              sf2.asInstanceOf[SignalFunction[Any, Any]]
            )
          ).asInstanceOf[SignalFunction[C, E]]
        case _ =>
          ComposeSF[C, D, E](sf1, sf2)
      }

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
}

