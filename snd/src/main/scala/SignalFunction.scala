import scalaz.Arrow
import scalaz.syntax.arrow._
import scala.specialized

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.existentials

sealed trait SignalFunction[-A, +B] {
  def buildProc(): A => B

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
  override def buildProc() = sf.buildProc()
  override def toString() =
    s"[${name}]"
}

case class FunctionSF[@specialized(Double, Int) A, @specialized(Double, Int) B](f: A => B) extends SignalFunction[A, B] {
  override def buildProc(): A => B = f
  override def toString =
    "[function]"
}
case class IdSF[A]() extends SignalFunction[A, A] {
  override def buildProc(): A => A = identity
  override def toString =
    "[id]"
}
case class BypassSF[A, B, C](sf: SignalFunction[A, B]) extends SignalFunction[(A, C), (B, C)] {
  override def buildProc(): ((A, C)) => (B, C) = {
    val proc = sf.buildProc()
    ac: (A, C) => (proc(ac._1) -> ac._2)
  }

  override def toString =
    s"(${sf}) *** [bypass]"
}
case class ComposeSF[A, B, C](sfBC: SignalFunction[B, C], sfAB: SignalFunction[A, B]) extends SignalFunction[A, C] {
  override def buildProc(): A => C =
    sfAB.buildProc() andThen sfBC.buildProc()

  override def toString =
    s"(${sfAB}) >>> (${sfBC})"
}
case class StatefulSF[A, B, C](sf: SignalFunction[(A, C), (B, C)], init: C) extends SignalFunction[A, B] {
  override def buildProc(): A => B = {
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
  override def buildProc(): Unit => A =
    _ => value
  override def toString =
    s"[consst(${value}]"
}

object SignalFunction {
  implicit def arrowInstance: ArrowDelayLoop[SignalFunction] = new ArrowDelayLoop[SignalFunction] {
    override def arr[C, D](f: C => D): SignalFunction[C, D] =
      FunctionSF(f)

    override def id[C]: SignalFunction[C, C] =
      IdSF[C]()

    override def first[C, D, E](sf: SignalFunction[C, D]): SignalFunction[(C, E), (D, E)] =
      BypassSF[C, D, E](sf)

    override def compose[C, D, E](sf1: SignalFunction[D, E], sf2: SignalFunction[C, D]): SignalFunction[C, E] =
      ComposeSF[C, D, E](sf1, sf2)

    override def delayLoop[A, B, C](init: C, a: SignalFunction[(A, C), (B, C)]): SignalFunction[A, B] =
      StatefulSF(a, init)
  }

  implicit def autoConst[A](a: A): SignalFunction[Unit, A] =
    const(a)

  def apply[A, B](f: A => B): SignalFunction[A, B] =
    FunctionSF[A, B](f)

  def stateful[A, B, @specialized(Int, Long, Double) C](init: C)(f: (A, C) => (B, C)): StatefulSF[A, B, C] =
    StatefulSF(FunctionSF[(A, C), (B, C)](_ match { case (a, c) => f(a, c) }), init)

  def stateOut[A, @specialized(Int, Long, Double) B](init: B)(f: (A, B) => B): StatefulSF[A, B, B] =
    stateful[A, B, B](init) {
      case (a, b) =>
        val b_ = f(a, b)
        (b_ -> b_)
    }

  def id[A]: SignalFunction[A, A] = IdSF[A]

  val unit: ConstSF[Unit] = ConstSF(())

  def const[A](value: A): ConstSF[A] = ConstSF(value)

  def ignore[A]: SignalFunction[A, Unit] =
    SignalFunction[A, Unit] { _ => () }.name("ignore")

  def delay[A](init: A): SignalFunction[A, A] =
    stateOut[A, A](init) { case (a, s) => a }
}

