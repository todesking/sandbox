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
        prove[Times[_2, _0, _0]](`T-Succ`(`T-Succ`(prove[Times[_0, _0, _0]], prove[Plus[_0, _0, _0]]), prove[Plus[_0, _0, _0]])),
        prove[Times[_2, _1, _2]](`T-Succ`(`T-Succ`(prove[Times[_0, _1, _0]], prove[Plus[_1, _0, _1]]), prove[Plus[_1, _1, _2]]))
      )
    }
    describe("CompareNat1") {
      import GenericNum._
      import CompareNat._
      import CompareNat1._
      Seq(
        prove[LessThan[_2, _3]],
        prove[LessThan[_2, _5]](`L-Trans`(prove[LessThan[_2, _3]], `L-Trans`(prove[LessThan[_3, _4]], prove[LessThan[_4, _5]])))
      )
    }
    describe("CompareNat2") {
      import GenericNum._
      import CompareNat._
      import CompareNat2._
      Seq(
        prove[LessThan[_2, _3]](`L-SuccSucc`(`L-SuccSucc`(`L-Zero`[_0]))),
        prove[LessThan[_2, _5]](`L-SuccSucc`(`L-SuccSucc`(`L-Zero`[_2])))
      )
    }
    describe("CompareNat3") {
      import GenericNum._
      import CompareNat._
      import CompareNat3._
      Seq(
        prove[LessThan[_2, _3]],
        prove[LessThan[_2, _5]](`L-SuccR`(`L-SuccR`(prove[LessThan[_2, _3]])))
      )
    }
    describe("EvalNatExp") {
      import GenericNum._
      import Nat._
      import EvalNatExp._
      Seq(
        prove[(E_0 + E_2) ⇓ E_2](
          `E-Plus`(
            prove[E_0 ⇓ E_0],
            prove[E_2 ⇓ E_2],
            prove[Plus[_0, _2, _2]])),
        prove[(E_2 + E_0) ⇓ E_2](
          `E-Plus`(
            prove[E_2 ⇓ E_2],
            prove[E_0 ⇓ E_0],
            prove[Plus[_2, _0, _2]])),
        prove[(E_1 + E_1 + E_1) ⇓ E_3](
          `E-Plus`(
            `E-Plus`(
              prove[E_1 ⇓ E_1],
              prove[E_1 ⇓ E_1],
              prove[Plus[_1, _1, _2]]),
            prove[E_1 ⇓ E_1],
            prove[Plus[_2, _1, _3]]
          )
        ),
        prove[(E_3 + (E_2 * E_1)) ⇓ E_5](
          `E-Plus`(
            `E-Const`[_3],
            `E-Times`(
              `E-Const`[_2],
              `E-Const`[_1],
              `T-Succ`(
                `T-Succ`(
                  `T-Zero`[_1],
                  prove[Plus[_1, _0, _1]]),
                prove[Plus[_1, _1, _2]])),
            prove[Plus[_3, _2, _5]])
        ),
        prove[((E_2 + E_2) * E_0) ⇓ E_0](
          `E-Times`(
            `E-Plus`(
              `E-Const`[_2],
              `E-Const`[_2],
              prove[Plus[_2, _2, _4]]),
            `E-Const`[_0],
            `T-Succ`(
              `T-Succ`(
                `T-Succ`(
                  `T-Succ`(
                    `T-Zero`[_0],
                    prove[Plus[_0, _0, _0]]),
                  prove[Plus[_0, _0, _0]]),
                prove[Plus[_0, _0, _0]]),
              prove[Plus[_0, _0, _0]])
          )
        ),
        prove[(E_0 * (E_2 + E_2)) ⇓ E_0](
          `E-Times`(
            `E-Const`[_0],
            `E-Plus`(`E-Const`[_2], `E-Const`[_2], prove[Plus[_2, _2, _4]]),
            `T-Zero`[_4]
          )
        )
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
  trait Num
  trait S[A <: Num] extends Num
  trait Z extends Num

  object Num {
    class IntRepr[A <: Num](val value: Int) extends Repr[A](value.toString)
    implicit val reprZ: Repr[Z] = new IntRepr(0)
    implicit def repr[N <: Num: Repr]: IntRepr[S[N]] =
      new IntRepr(implicitly[Repr[N]].asInstanceOf[IntRepr[N]].value + 1)
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
    `T-Succ`[N1, N2, N3, N4](implicitly[Repr[N1]], implicitly[Repr[N2]], implicitly[Repr[N4]], ev1, ev2)

  implicit def `T-Succ`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num, N4 <: Num: Repr](implicit ev1: Times[N1, N2, N3], ev2: Plus[N2, N3, N4]): Times[S[N1], N2, N4] =
    new Times[S[N1], N2, N4] { override def provenBy = "T-Succ"; override def assumptions = Seq(ev1, ev2) }
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
  def `L-Trans`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr](ev1: LessThan[N1, N2], ev2: LessThan[N2, N3]): LessThan[N1, N3] =
    `L-Trans`[N1, N2, N3](implicitly[Repr[N1]], implicitly[Repr[N2]], implicitly[Repr[N3]], ev1, ev2)
  implicit def `L-Trans`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr](implicit ev1: LessThan[N1, N2], ev2: LessThan[N2, N3]): LessThan[N1, N3] =
    new LessThan[N1, N3] { override def provenBy = "L-Trans"; override def assumptions = Seq(ev1, ev2) }
}

object CompareNat2 {
  import GenericNum._
  import CompareNat._

  implicit def `L-Zero`[N <: Num: Repr]: LessThan[Z, S[N]] =
    new LessThan[Z, S[N]] { override def provenBy = "L-Zero" }
  implicit def `L-SuccSucc`[N1 <: Num: Repr, N2 <: Num: Repr](ev: LessThan[N1, N2]): LessThan[S[N1], S[N2]] =
    new LessThan[S[N1], S[N2]] { override def provenBy = "L-SuccSucc"; override def assumptions = Seq(ev) }
}

object CompareNat3 {
  import GenericNum._
  import CompareNat._

  implicit def `L-Succ`[N <: Num: Repr]: LessThan[N, S[N]] =
    new LessThan[N, S[N]] { override def provenBy = "L-Succ" }
  implicit def `L-SuccR`[N1 <: Num: Repr, N2 <: Num: Repr](ev: LessThan[N1, N2]): LessThan[N1, S[N2]] =
    new LessThan[N1, S[N2]] { override def provenBy = "L-SuccR"; override def assumptions = Seq(ev) }
}

object EvalNatExp {
  import GenericNum._
  import Nat._
  import Repr.repr

  trait Exp
  object Exp {
    implicit def reprN[A <: Num: Repr]: Repr[ENum[A]] = new Repr(repr[A].toString)

    implicit def repr_*[E1 <: Exp: Repr, E2 <: Exp: Repr]: Repr[E1 * E2] =
      new Repr(s"(${repr[E1]} * ${repr[E2]})")

    implicit def repr_+[E1 <: Exp: Repr, E2 <: Exp: Repr]: Repr[E1 + E2] =
      new Repr(s"(${repr[E1]} + ${repr[E2]})")
  }

  abstract class ENum[A <: Num: Repr] extends Exp
  abstract class +[E1 <: Exp: Repr, E2 <: Exp: Repr] extends Exp
  abstract class *[E1 <: Exp: Repr, E2 <: Exp: Repr] extends Exp

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
}
