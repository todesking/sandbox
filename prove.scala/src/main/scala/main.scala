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
      import CompareNat1._
      Seq(
        prove[LessThan[_2, _3]],
        prove[LessThan[_2, _5]]
      )
    }
    describe("CompareNat2") {
      import CompareNat2._
      Seq(
        prove[LessThan[_2, _3]],
        prove[LessThan[_2, _5]]
      )
    }
    describe("CompareNat3") {
      import CompareNat3._
      Seq(
        prove[LessThan[_2, _3]],
        prove[LessThan[_2, _5]]
      )
    }
    describe("EvalNatExp") {
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
    describe("ReduceNatExp") {
      import ReduceNatExp._
      Seq(
        prove[(E_0 + E_2) -*-> E_2],
        prove[((E_1 * E_1) + (E_1 * E_1)) `-d->` (E_1 + (E_1 * E_1))],
        prove[((E_1 * E_1) + (E_1 * E_1)) `--->` ((E_1 * E_1) + E_1)](
          `R-PlusR`(prove[(E_1 * E_1) ---> E_1])), // -*-> is nondeterministic
        prove[((E_1 * E_1) + (E_1 * E_1)) `-*->` E_2](
          `MR-Multi`(
            `MR-Multi`(
              prove[((E_1 * E_1) + (E_1 * E_1)) `-*->` (E_1 + (E_1 * E_1))](
                `MR-One`(`R-PlusL`(prove[(E_1 * E_1) ---> E_1]))),
              prove[(E_1 + (E_1 * E_1)) `-*->` (E_1 + E_1)](
                `MR-One`(`R-PlusR`(prove[(E_1 * E_1) ---> E_1]))
              )
            ),
            prove[(E_1 + E_1) -*-> E_2]
          )
        )
      )
    }
    describe("EvalML1") {
      import EvalML1._
      Seq(
        prove[E_0 ⇓ E_0],
        prove[ETrue ⇓ ETrue],
        prove[If[ETrue, E_0, E_1] ⇓ E_0],
        prove[If[EFalse, E_0, E_1] ⇓ E_1],
        prove[(E_0 + E_0) ⇓ E_0],
        prove[((E_2 * E_3) - E_1) ⇓ E_5]
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


trait GenericNum {
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

  object NumOperators {
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
trait GenericBool {
  trait Bool
  trait True extends Bool
  trait False extends Bool
  object Bool {
    implicit val reprTrue: Repr[True] = new Repr("true")
    implicit val reprFalse: Repr[False] = new Repr("false")
  }
}

object Nat extends Nat
trait Nat extends GenericNum {
  import NumOperators._
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

trait NatMinus extends Nat {
  import NumOperators._
  import Repr.repr

  abstract class Minus[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr] extends Proof {
    override def str = s"${repr[N1]} minus ${repr[N2]} is ${repr[N3]}"
  }

  implicit def `M-Zero`[N1 <: Num: Repr]: Minus[N1, Z, N1] =
    new Minus[N1, Z, N1] { override def provenBy = "M-Zero" }

  implicit def `M-Succ`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr](implicit ev1: Minus[N1, N2, S[N3]]): Minus[N1, S[N2], N3] =
    new Minus[N1, S[N2], N3] { override def provenBy = "M-Succ"; override def assumptions = Seq(ev1) }
}

trait GenericCompareNat extends GenericNum {
  import Repr.repr
  abstract class LessThan[N1 <: Num: Repr, N2 <: Num: Repr] extends Proof {
    override def str = s"${repr[N1]} is less than ${repr[N2]}"
  }
}

object CompareNat1 extends CompareNat1
trait CompareNat1 extends GenericCompareNat {
  implicit def `L-Succ`[N1 <: Num: Repr]: LessThan[N1, S[N1]] =
    new LessThan[N1, S[N1]] { override def provenBy = "L-Succ" }

  def `L-Trans`[N1 <: Num: Repr, N2 <: Num, N3 <: Num: Repr](ev1: LessThan[N1, N2], ev2: LessThan[N2, N3]): LessThan[N1, N3] =
    new LessThan[N1, N3] { override def provenBy = "L-Trans"; override def assumptions = Seq(ev1, ev2) }

  // diverge: (ev1: LessThan[N1, N3#Pred], ev2: LessThan[N3#Pred, N3]): LessThan[N1, N3]
  // error: (ev1: LessThan[N1, N3#Pred]):LessThan[N1, N3]
  implicit def `prove-L-Trans`[N1 <: Num: Repr, N3 <: Num: Repr](implicit ev1: LessThan[N1, N3]): LessThan[N1, S[N3]] =
    `L-Trans`(ev1, `L-Succ`[N3])
}

object CompareNat2 extends GenericCompareNat {
  implicit def `L-Zero`[N <: Num: Repr]: LessThan[Z, S[N]] =
    new LessThan[Z, S[N]] { override def provenBy = "L-Zero" }
  implicit def `L-SuccSucc`[N1 <: Num: Repr, N2 <: Num: Repr](implicit ev: LessThan[N1, N2]): LessThan[S[N1], S[N2]] =
    new LessThan[S[N1], S[N2]] { override def provenBy = "L-SuccSucc"; override def assumptions = Seq(ev) }
}

object CompareNat3 extends GenericCompareNat {
  implicit def `L-Succ`[N <: Num: Repr]: LessThan[N, S[N]] =
    new LessThan[N, S[N]] { override def provenBy = "L-Succ" }
  implicit def `L-SuccR`[N1 <: Num: Repr, N2 <: Num: Repr](implicit ev: LessThan[N1, N2]): LessThan[N1, S[N2]] =
    new LessThan[N1, S[N2]] { override def provenBy = "L-SuccR"; override def assumptions = Seq(ev) }
}

trait GenericExp extends GenericNum {
  import Repr.repr

  type E_0 = ENum[_0]
  type E_1 = ENum[_1]
  type E_2 = ENum[_2]
  type E_3 = ENum[_3]
  type E_4 = ENum[_4]
  type E_5 = ENum[_5]

  trait Exp {
    type Self <: Exp
    type Result <: Exp
    type ToNum <: Num
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
}

object EvalNatExp extends Nat with GenericExp {
  import Repr.repr

  abstract class Eval[E1 <: Exp: Repr, E2 <: Exp: Repr] extends Proof {
    override def str = s"${repr[E1]} ⇓ ${repr[E2]}"
  }
  type ⇓[A <: Exp, B <: Exp] = Eval[A, B]
  type ToEval[A <: Exp] = Eval[A, A#Result]

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
    eq: ToEval[E1 + E2] =:= ((E1 + E2) ⇓ E3)
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
    eq: ToEval[E1 * E2] =:= ((E1 * E2) ⇓ E3)
  ): (E1 * E2) ⇓ E3 =
    eq(`E-Times`(ev1, ev2, ev3))

}

object ReduceNatExp extends Nat with GenericExp {
  import Repr.repr

  abstract class -*->[A <: Exp: Repr, B <: Exp: Repr] extends Proof {
    override def str = s"${repr[A]} -*-> ${repr[B]}"
  }

  abstract class --->[A <: Exp: Repr, B <: Exp: Repr] extends Proof {
    override def str = s"${repr[A]} ---> ${repr[B]}"
  }

  abstract class `-d->`[A <: Exp: Repr, B <: Exp: Repr] extends Proof {
    override def str = s"${repr[A]} -d-> ${repr[B]}"
  }

  implicit def `R-Plus`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr](implicit ev1: Plus[N1, N2, N3]): (ENum[N1] + ENum[N2]) ---> ENum[N3] =
    new --->[ENum[N1] + ENum[N2], ENum[N3]] { override def provenBy = "R-Plus"; override def assumptions = Seq(ev1) }

  implicit def `R-Times`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr](implicit ev1: Times[N1, N2, N3]): (ENum[N1] * ENum[N2]) ---> ENum[N3] =
    new --->[ENum[N1] * ENum[N2], ENum[N3]] { override def provenBy = "R-Times"; override def assumptions = Seq(ev1) }

  def `R-PlusL`[E1 <: Exp: Repr, E2 <: Exp: Repr, `E1'` <: Exp: Repr](ev1: E1 ---> `E1'`): (E1 + E2) ---> (`E1'` + E2) =
    new --->[E1 + E2, `E1'` + E2] { override def provenBy = "R-PlusL"; override def assumptions = Seq(ev1) }

  def `R-PlusR`[E1 <: Exp: Repr, E2 <: Exp: Repr, `E2'` <: Exp: Repr](ev1: E2 ---> `E2'`): (E1 + E2) ---> (E1 + `E2'`) =
    new --->[E1 + E2, E1 + `E2'`] { override def provenBy = "R-PlusR"; override def assumptions = Seq(ev1) }

  def `R-TimesL`[E1 <: Exp: Repr, E2 <: Exp: Repr, `E1'` <: Exp: Repr](ev1: E1 ---> `E1'`): (E1 * E2) ---> (`E1'` * E2) =
    new --->[E1 * E2, `E1'` * E2] { override def provenBy = "R-TimesL"; override def assumptions = Seq(ev1) }

  def `R-TimesR`[E1 <: Exp: Repr, E2 <: Exp: Repr, `E2'` <: Exp: Repr](ev1: E2 ---> `E2'`): (E1 * E2) ---> (E1 * `E2'`) =
    new --->[E1 * E2, E1 * `E2'`] { override def provenBy = "R-TimesR"; override def assumptions = Seq(ev1) }

  implicit def `DR-Plus`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr](implicit ev1: Plus[N1, N2, N3]): (ENum[N1] + ENum[N2]) `-d->` ENum[N3] =
    new `-d->`[ENum[N1] + ENum[N2], ENum[N3]] { override def provenBy = "DR-Plus"; override def assumptions = Seq(ev1) }

  implicit def `DR-Times`[N1 <: Num: Repr, N2 <: Num: Repr, N3 <: Num: Repr](implicit ev1: Times[N1, N2, N3]): (ENum[N1] * ENum[N2]) `-d->` ENum[N3] =
    new `-d->`[ENum[N1] * ENum[N2], ENum[N3]] { override def provenBy = "DR-Times"; override def assumptions = Seq(ev1) }

  implicit def `DR-PlusL`[E1 <: Exp: Repr, E2 <: Exp: Repr, `E1'` <: Exp: Repr](implicit ev1: E1 `-d->` `E1'`): (E1 + E2) `-d->` (`E1'` + E2) =
    new `-d->`[E1 + E2, `E1'` + E2] { override def provenBy = "DR-PlusL"; override def assumptions = Seq(ev1) }

  implicit def `DR-PlusR`[E1 <: Exp: Repr, E2 <: Exp: Repr, `E2'` <: Exp: Repr](implicit ev1: E2 `-d->` `E2'`): (E1 + E2) `-d->` (E1 + `E2'`) =
    new `-d->`[E1 + E2, E1 + `E2'`] { override def provenBy = "DR-PlusR"; override def assumptions = Seq(ev1) }

  implicit def `DR-TimesL`[E1 <: Exp: Repr, E2 <: Exp: Repr, `E1'` <: Exp: Repr](implicit ev1: E1 `-d->` `E1'`): (E1 * E2) `-d->` (`E1'` * E2) =
    new `-d->`[E1 * E2, `E1'` * E2] { override def provenBy = "DR-TimesL"; override def assumptions = Seq(ev1) }

  implicit def `DR-TimesR`[E1 <: Exp: Repr, E2 <: Exp: Repr, `E2'` <: Exp: Repr](implicit ev1: E2 `-d->` `E2'`): (E1 * E2) `-d->` (E1 * `E2'`) =
    new `-d->`[E1 * E2, E1 * `E2'`] { override def provenBy = "DR-TimesR"; override def assumptions = Seq(ev1) }

  def `MR-Zero`[E <: Exp: Repr]: E -*-> E =
    new -*->[E, E] { override def provenBy = "MR-Zero" }

  def `MR-Multi`[E0 <: Exp: Repr, E1 <: Exp: Repr, E2 <: Exp: Repr](ev1: E0 -*-> E1, ev2: E1 -*-> E2): E0 -*-> E2 =
    new -*->[E0, E2] { override def provenBy = "MR-Multi"; override def assumptions = Seq(ev1, ev2) }

  def `MR-One`[E0 <: Exp: Repr, E1 <: Exp: Repr](ev1: E0 ---> E1) : E0 -*-> E1 = {
    implicit val e1 = ev1
    `MR-One`[E0, E1]
  }
  implicit def `MR-One`[E0 <: Exp: Repr, E1 <: Exp: Repr](implicit ev1: E0 ---> E1) : E0 -*-> E1 =
    new -*->[E0, E1] { override def provenBy = "MR-One"; override def assumptions = Seq(ev1) }
}

object EvalML1 extends NatMinus with CompareNat1 with GenericBool {
  import Repr.repr

  trait Exp {
    type EvalI = int[EvalNum]
    type EvalNum <: Num
    type ToNum = EvalNum
  }

  trait Value extends Exp
  trait int[N <: Num] extends Value {
    override type EvalNum = N
  }
  trait bool[B <: Bool] extends Value {
  }

  trait Op[P <: Prim, E1 <: Exp, E2 <: Exp] extends Exp {
  }
  trait Prim {
    type Apply[E1 <: Exp, E2 <: Exp] <: Exp
  }
  trait Prim_+ extends Prim {
    override type Apply[E1 <: Exp, E2 <: Exp] = int[E1#EvalNum#Plus[E2#EvalNum]]
  }
  trait Prim_- extends Prim
  trait Prim_* extends Prim
  trait Prim_< extends Prim
  type +[A <: Exp, B <: Exp] = Op[Prim_+, A, B]
  type -[A <: Exp, B <: Exp] = Op[Prim_-, A, B]
  type *[A <: Exp, B <: Exp] = Op[Prim_*, A, B]
  type <[A <: Exp, B <: Exp] = Op[Prim_<, A, B]

  trait If[E1 <: Exp, E2 <: Exp, E3 <: Exp] extends Exp

  type ETrue = bool[True]
  type EFalse = bool[False]

  object Exp {
    implicit def reprInt[N <: Num: Repr]: Repr[int[N]] = new Repr[int[N]](repr[N].toString)
    implicit def reprBool[B <: Bool: Repr]: Repr[bool[B]] = new Repr[bool[B]](repr[B].toString)
    implicit def reprIf[E1 <: Exp: Repr, E2 <: Exp: Repr, E3 <: Exp: Repr]: Repr[If[E1, E2, E3]] =
      new Repr(s"if (${repr[E1]}) then (${repr[E2]}) else (${repr[E3]})")
    implicit def reprOp[P <: Prim: Repr, E1 <: Exp: Repr, E2 <: Exp: Repr]: Repr[Op[P, E1, E2]] =
      new Repr(s"(${repr[E1]}) ${repr[P]} (${repr[E2]})")
  }
  object Prim {
    implicit val reprPrim_+ : Repr[Prim_+] = new Repr("+")
    implicit val reprPrim_- : Repr[Prim_-] = new Repr("-")
    implicit val reprPrim_* : Repr[Prim_*] = new Repr("*")
    implicit val reprPrim_< : Repr[Prim_<] = new Repr("<")
  }

  abstract class Eval[E1 <: Exp: Repr, E2 <: Exp: Repr] extends Proof {
    override def str = s"${repr[E1]} ⇓ ${repr[E2]}"
  }
  type ⇓[A <: Exp, B <: Exp] = Eval[A, B]

  type E_0 = int[_0]
  type E_1 = int[_1]
  type E_2 = int[_2]
  type E_3 = int[_3]
  type E_4 = int[_4]
  type E_5 = int[_5]

  implicit def `E-Int`[I <: int[_]: Repr]: I ⇓ I =
    new Eval[I, I] { override def provenBy = "E-Int" }
  implicit def `E-Bool`[B <: bool[_]: Repr]: B ⇓ B =
    new Eval[B, B] { override def provenBy = "E-Bool" }
  implicit def `E-IfT`[E1 <: Exp: Repr, E2 <: Exp: Repr, E3 <: Exp: Repr, V <: Value: Repr](implicit ev1: E1 ⇓ ETrue, ev2: E2 ⇓ V): If[E1, E2, E3] ⇓ V =
    new Eval[If[E1, E2, E3], V] { override def provenBy = "E-IfT"; override def assumptions = Seq(ev1, ev2) }
  implicit def `E-IfF`[E1 <: Exp: Repr, E2 <: Exp: Repr, E3 <: Exp: Repr, V <: Value: Repr](implicit ev1: E1 ⇓ EFalse, ev2: E3 ⇓ V): If[E1, E2, E3] ⇓ V =
    new Eval[If[E1, E2, E3], V] { override def provenBy = "E-IfF"; override def assumptions = Seq(ev1, ev2) }

  def `E-Plus`[E1 <: Exp: Repr, N1 <: Num, I1 <: int[N1], E2 <: Exp: Repr, N2 <: Num, I2 <: int[N2], N3 <: Num, I3 <: int[N3]: Repr](
    ev1: E1 ⇓ I1,
    ev2: E2 ⇓ I2,
    ev3: Plus[I1#ToNum, I2#ToNum, I3#ToNum]
  ): (E1 + E2) ⇓ I3 =
    new Eval[E1 + E2, I3] { override def provenBy = "E-Plus"; override def assumptions = Seq(ev1, ev2, ev3) }

  implicit def `prove-E-Plus`[E1 <: Exp: Repr, E2 <: Exp: Repr, N3 <: Num, I3 <: int[N3]: Repr](
    implicit
    ev1: E1 ⇓ E1#EvalI,
    ev2: E2 ⇓ E2#EvalI,
    ev3: Plus[E1#EvalI#ToNum, E2#EvalI#ToNum, N3]
  ): (E1 + E2) ⇓  I3 =
    `E-Plus`[E1, E1#EvalI#ToNum, int[E1#EvalI#ToNum], E2, E2#EvalI#ToNum, int[E2#EvalI#ToNum], N3, I3](ev1, ev2, ev3)

  def `E-Times`[E1 <: Exp: Repr, I1 <: int[_ <: Num], E2 <: Exp: Repr, I2 <: int[_ <: Num], I3 <: int[_ <: Num]: Repr](
    ev1: E1 ⇓ I1,
    ev2: E2 ⇓ I2,
    ev3: Times[I1#ToNum, I2#ToNum, I3#ToNum]
  ): (E1 * E2) ⇓ I3 =
    new Eval[E1 * E2, I3] { override def provenBy = "E-Times"; override def assumptions = Seq(ev1, ev2, ev3) }
  implicit def `E-Minus`[E1 <: Exp: Repr, I1 <: int[_ <: Num], E2 <: Exp: Repr, I2 <: int[_ <: Num], I3 <: int[_ <: Num]: Repr](
    implicit
    ev1: E1 ⇓ I1,
    ev2: E2 ⇓ I2,
    ev3: Minus[I1#ToNum, I2#ToNum, I3#ToNum]
  ): (E1 - E2) ⇓ I3 =
    new Eval[E1 - E2, I3] { override def provenBy = "E-Plus"; override def assumptions = Seq(ev1, ev2, ev3) }
  implicit def `E-LT`[E1 <: Exp: Repr, I1 <: int[_ <: Num], E2 <: Exp: Repr, I2 <: int[_ <: Num]](
    implicit
    ev1: E1 ⇓ I1,
    ev2: E2 ⇓ I2,
    ev3: LessThan[I1#ToNum, I2#ToNum]
  ): (E1 < E2) ⇓ ETrue =
    new Eval[E1 < E2, ETrue] { override def provenBy = "E-LT"; override def assumptions = Seq(ev1, ev2, ev3) }
  def `E-LT-GT`[E1 <: Exp: Repr, I1 <: int[_ <: Num], E2 <: Exp: Repr, I2 <: int[_ <: Num]](
    ev1: E1 ⇓ I1,
    ev2: E2 ⇓ I2,
    ev3: LessThan[I2#ToNum, I1#ToNum]
  ): (E1 < E2) ⇓ EFalse =
    new Eval[E1 < E2, EFalse] { override def provenBy = "E-LT-GT"; override def assumptions = Seq(ev1, ev2, ev3) }
  def `E-LT-EQ`[E1 <: Exp: Repr, I1 <: int[_ <: Num], E2 <: Exp: Repr, I2 <: int[_ <: Num]](
    ev1: E1 ⇓ I1,
    ev2: E2 ⇓ I2,
    ev3: I1 =:= I2
  ): (E1 < E2) ⇓ EFalse =
    new Eval[E1 < E2, EFalse] { override def provenBy = "E-LT-GT"; override def assumptions = Seq(ev1, ev2) }
}
