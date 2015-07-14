import scala.language.higherKinds
import scala.language.implicitConversions

object Main {
  def describe(name: String)(proofs: =>Seq[Proof]): Unit = {
    println(s"=== ${name} ===")
    proofs.foreach { p => println(p.pretty()); println() }
  }

  def main(args: Array[String]): Unit = {
    import Proof.prove
    describe("Nat") {
      import GenericNum._
      import Nat._
      Seq(
        prove[Plus[_0, _0, _0]],
        prove[Plus[_0, _2, _2]],
        prove[Plus[_2, _0, _2]],
        prove[Plus[_2, _1, _3]],
        prove[Plus[_1, _3, _4]],
        prove[Times[_0, _2, _0]],
        prove[Times[_1, _0, _0]],
        prove[Times[_1, _1, _1]],
        prove[Times[_2, _0, _0]],
        prove[Times[_2, _1, _2]]
      )
    }
    describe("CompareNat1") {
      import GenericNum._
      import CompareNat._
      import CompareNat1._
      Seq(
        prove[LessThan[_2, _3]],
        prove[LessThan[_2, _5]]
      )
    }
    describe("CompareNat2") {
      import GenericNum._
      import CompareNat._
      import CompareNat2._
      Seq(
        prove[LessThan[_2, _3]],
        prove[LessThan[_2, _5]]
      )
    }
    describe("CompareNat3") {
      import GenericNum._
      import CompareNat._
      import CompareNat3._
      Seq(
        prove[LessThan[_2, _3]],
        prove[LessThan[_2, _5]]
      )
    }
    describe("EvalNatExp") {
      import GenericNum._
      import Nat._
      import EvalNatExp._
      Seq(
        prove[E_0 ⇓ E_0],
        prove[(E_0 + E_2) ⇓ E_2],
        prove[(E_2 + E_0) ⇓ E_2],
        prove[(E_1 + E_1 + E_1) ⇓ E_3],
        prove[(E_3 + (E_2 * E_1)) ⇓ E_5],
        prove[((E_2 + E_2) * E_0) ⇓ E_0],
        prove[(E_0 * (E_2 + E_2)) ⇓ E_0]
      )
    }
  }
}

trait Proof {
  def provenBy: String
  def assumptions: Seq[Proof] = Seq.empty
  def str: String
  override def toString = s"${str} by ${provenBy}"
  def pretty(indent: Int = 0): String = (assumptions.map(_.pretty(indent + 1)) :+ ("  " * indent + toString)).mkString("\n")
}

object Proof {
  def prove[A <: Proof](implicit ev: A): A = ev
}

class Repr[A](str: String) {
  override def toString =str
}

object Repr {
  def repr[A: Repr]: Repr[A] = implicitly[Repr[A]]
}


object GenericNum {
  trait Num {
    type Minus[A <: Num] <: Num
    type Plus[A <: Num] <: Num
    type Times[A <: Num] <: Num
    type IfZero[T, Then <: T, Else <: T] <: T
    type Pred <: Num
    type Succ <: Num
  }
  trait S[A <: Num] extends Num {
    override type Minus[B <: Num] = B#IfZero[Num, S[A], A#Minus[B#Pred]]
    override type Plus[B <: Num] = A#Plus[S[B]]
    override type Times[B <: Num] = B#Plus[A#Times[B]]
    override type IfZero[T, Then <: T, Else <: T] = Else
    override type Pred = A
    override type Succ = S[S[A]]
  }
  trait Z extends Num {
    override type Minus[B <: Num] = B#IfZero[Num, Z, Nothing]
    override type Times[A <: Num] = Z
    override type Plus[B <: Num] = B
    override type IfZero[T, Then <: T, Else <: T] = Then
    override type Succ = S[Z]
  }

  object Operators {
    type -[A <: Num, B <: Num] = A#Minus[B]
    type +[A <: Num, B <: Num] = A#Plus[B]
  }

  object Num {
    implicit val reprZ: Repr[Z] = new Repr("0")
    implicit def repr[N <: Num: Repr]: Repr[S[N]] =
      new Repr[S[N]]((implicitly[Repr[N]].toString.toInt + 1).toString)
  }

  type _0 = Z
  type _1 = S[_0]
  type _2 = S[_1]
  type _3 = S[_2]
  type _4 = S[_3]
  type _5 = S[_4]
}

object Nat {
  import GenericNum._
  import GenericNum.Operators._
  import Repr.repr

  abstract class Plus[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr] extends Proof {
    override def str = s"${repr[N1]} plus ${repr[N2]} is ${repr[N3]}"
  }
  abstract class Times[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr] extends Proof {
    override def str = s"${repr[N1]} times ${repr[N2]} is ${repr[N3]}"
  }

  implicit def `P-Zero`[N <: Num: Repr]: Plus[Z, N, N] =
    new Plus[Z, N, N] { override def provenBy = "P-Zero" }

  implicit def `P-Succ`[N1 <: Num: Repr, N2 <: Num: Repr, N <: Num: Repr](implicit ev: Plus[N1, N2, N]): Plus[S[N1], N2, S[N]] =
    new Plus[S[N1], N2, S[N]] { override def provenBy = "P-Succ"; override def assumptions = Seq(ev) }

  implicit def `T-Zero`[N <: Num: Repr]: Times[Z, N, Z] =
    new Times[Z, N, Z] { override def provenBy = "T-Zero" }

  def `T-Succ`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num, N4 <: Num: Repr](ev1: Times[N1, N2, N3], ev2: Plus[N2, N3, N4]): Times[S[N1], N2, N4] =
    new Times[S[N1], N2, N4] { override def provenBy = "T-Succ"; override def assumptions = Seq(ev1, ev2) }

  implicit def `prove-T-Succ`[N1 <: Num: Repr, N2 <: Num: Repr, N4 <: Num: Repr](implicit ev1: Times[N1, N2, N4 - N2], ev2: Plus[N2, N4 - N2, N4]): Times[S[N1], N2, N4] =
    `T-Succ`(ev1, ev2)
}

object CompareNat {
  import GenericNum._
  import Repr.repr
  abstract class LessThan[N1 <: Num: Repr, N2 <: Num: Repr] extends Proof {
    override def str = s"${repr[N1]} is less than ${repr[N2]}"
  }
}

object CompareNat1 {
  import GenericNum._
  import CompareNat._

  implicit def `L-Succ`[N1 <: Num: Repr]: LessThan[N1, S[N1]] =
    new LessThan[N1, S[N1]] { override def provenBy = "L-Succ" }

  def `L-Trans`[N1 <: Num: Repr, N2 <: Num, N3 <: Num: Repr](ev1: LessThan[N1, N2], ev2: LessThan[N2, N3]): LessThan[N1, N3] =
    new LessThan[N1, N3] { override def provenBy = "L-Trans"; override def assumptions = Seq(ev1, ev2) }

  // diverge: (ev1: LessThan[N1, N3#Pred], ev2: LessThan[N3#Pred, N3]): LessThan[N1, N3]
  // error: (ev1: LessThan[N1, N3#Pred]):LessThan[N1, N3]
  implicit def `prove-L-Trans`[N1 <: Num: Repr, N3 <: Num: Repr](implicit ev1: LessThan[N1, N3]): LessThan[N1, S[N3]] =
    `L-Trans`(ev1, `L-Succ`[N3])
}

object CompareNat2 {
  import GenericNum._
  import CompareNat._

  implicit def `L-Zero`[N <: Num: Repr]: LessThan[Z, S[N]] =
    new LessThan[Z, S[N]] { override def provenBy = "L-Zero" }
  implicit def `L-SuccSucc`[N1 <: Num: Repr, N2 <: Num: Repr](implicit ev: LessThan[N1, N2]): LessThan[S[N1], S[N2]] =
    new LessThan[S[N1], S[N2]] { override def provenBy = "L-SuccSucc"; override def assumptions = Seq(ev) }
}

object CompareNat3 {
  import GenericNum._
  import CompareNat._

  implicit def `L-Succ`[N <: Num: Repr]: LessThan[N, S[N]] =
    new LessThan[N, S[N]] { override def provenBy = "L-Succ" }
  implicit def `L-SuccR`[N1 <: Num: Repr, N2 <: Num: Repr](implicit ev: LessThan[N1, N2]): LessThan[N1, S[N2]] =
    new LessThan[N1, S[N2]] { override def provenBy = "L-SuccR"; override def assumptions = Seq(ev) }
}

object EvalNatExp {
  import GenericNum._
  import Nat._
  import Repr.repr

  trait Exp {
    type Self <: Exp
    type Result <: Exp
    type ToNum <: Num
    type ToEval = Eval[Self, Result]
  }
  object Exp {
    implicit def reprN[A <: Num: Repr]: Repr[ENum[A]] = new Repr(repr[A].toString)

    implicit def repr_*[E1 <: Exp: Repr, E2 <: Exp: Repr]: Repr[E1 * E2] =
      new Repr(s"(${repr[E1]} * ${repr[E2]})")

    implicit def repr_+[E1 <: Exp: Repr, E2 <: Exp: Repr]: Repr[E1 + E2] =
      new Repr(s"(${repr[E1]} + ${repr[E2]})")
  }

  abstract class ENum[A <: Num: Repr] extends Exp {
    override type Self = ENum[A]
    override type ToNum = A
    override type Result = ENum[A]
  }
  abstract class +[E1 <: Exp: Repr, E2 <: Exp: Repr] extends Exp {
    override type Self = E1 + E2
    override type Result = ENum[E1#Result#ToNum#Plus[E2#Result#ToNum]]
  }
  abstract class *[E1 <: Exp: Repr, E2 <: Exp: Repr] extends Exp {
    override type Self = E1 * E2
    override type Result = ENum[E1#Result#ToNum#Times[E2#Result#ToNum]]
  }

  type E_0 = ENum[_0]
  type E_1 = ENum[_1]
  type E_2 = ENum[_2]
  type E_3 = ENum[_3]
  type E_4 = ENum[_4]
  type E_5 = ENum[_5]

  abstract class Eval[E1 <: Exp: Repr, E2 <: Exp: Repr] extends Proof {
    override def str = s"${repr[E1]} ⇓ ${repr[E2]}"
  }
  type ⇓[A <: Exp, B <: Exp] = Eval[A, B]

  implicit def `E-Const`[N <: Num: Repr]: ENum[N] ⇓ ENum[N] =
    new Eval[ENum[N], ENum[N]] { override def provenBy = "E-Const" }

  def `E-Plus`[
    E1 <: Exp: Repr, N1 <: Num: Repr,
    E2 <: Exp: Repr, N2 <: Num: Repr,
    N <: Num: Repr
  ](
    ev1: E1 ⇓ ENum[N1], ev2: E2 ⇓ ENum[N2], ev3: Plus[N1, N2, N]
  ): (E1 + E2) ⇓ ENum[N] =
    new Eval[E1 + E2, ENum[N]] {
      override def provenBy = "E-Plus"
      override def assumptions = Seq(ev1, ev2, ev3)
    }

  def `E-Times`[
    E1 <: Exp: Repr, N1 <: Num: Repr,
    E2 <: Exp: Repr, N2 <: Num: Repr,
    N <: Num: Repr
  ](
    ev1: E1 ⇓ ENum[N1], ev2: E2 ⇓ ENum[N2], ev3: Times[N1, N2, N]
  ): (E1 * E2) ⇓ ENum[N] =
    new Eval[E1 * E2, ENum[N]] {
      override def provenBy = "E-Times"
      override def assumptions = Seq(ev1, ev2, ev3)
    }

  // : (E1 + E2) ⇓ (E1 + E2)#Result だと探索に失敗する
  implicit def `prove-E-Plus`[E1 <: Exp: Repr, E2 <: Exp: Repr, E3 <: Exp: Repr](
    implicit
    ev1: E1 ⇓ ENum[E1#Result#ToNum],
    ev2: E2 ⇓ ENum[E2#Result#ToNum],
    ev3: Plus[E1#Result#ToNum, E2#Result#ToNum, E1#Result#ToNum#Plus[E2#Result#ToNum]],
    reprN1: Repr[E1#Result#ToNum],
    reprN2: Repr[E2#Result#ToNum],
    reprN3: Repr[E1#Result#ToNum#Plus[E2#Result#ToNum]],
    eq: (E1 + E2)#ToEval =:= ((E1 + E2) ⇓ E3)
  ): (E1 + E2) ⇓ E3 =
    eq(`E-Plus`(ev1, ev2, ev3))

  implicit def `prove-E-Times`[E1 <: Exp: Repr, E2 <: Exp: Repr, E3 <: Exp: Repr](
    implicit
    ev1: E1 ⇓ ENum[E1#Result#ToNum],
    ev2: E2 ⇓ ENum[E2#Result#ToNum],
    ev3: Times[E1#Result#ToNum, E2#Result#ToNum, E1#Result#ToNum#Times[E2#Result#ToNum]],
    reprN1: Repr[E1#Result#ToNum],
    reprN2: Repr[E2#Result#ToNum],
    reprN3: Repr[E1#Result#ToNum#Times[E2#Result#ToNum]],
    eq: (E1 * E2)#ToEval =:= ((E1 * E2) ⇓ E3)
  ): (E1 * E2) ⇓ E3 =
    eq(`E-Times`(ev1, ev2, ev3))

}
