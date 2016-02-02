package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{Method => JMethod}

import com.todesking.scalapp.syntax._

object Main {
  def main(args: Array[String]): Unit = {
  }
}

object Opt {
  def optimize[A <: AnyRef : ClassTag](orig: A): A = {
    val instance = Instance.Native[A](orig)
    instance.instance()
  }
}

class UnsupportedOpcodeException(byte: Int)
  extends RuntimeException(f"Unsupported opcode: 0x$byte%02X")

class AnalyzeException(msg: String) extends RuntimeException(msg)

object Util {
  def tsort[A, B](in: Seq[A])(labelOf: A => B)(depsOf: A => Set[B]): Seq[A] =
    tsort0(in.map { i => (i, labelOf(i), depsOf(i)) }, Set.empty, Seq.empty)

  private[this] def tsort0[A, B](in: Seq[(A, B, Set[B])], deps: Set[B], sorted: Seq[A]): Seq[A] =
    if(in.isEmpty) {
      sorted
    } else {
      val (nodep, dep) = in.partition { case (a, b, bs) => bs.forall(deps.contains) }
      tsort0(dep, deps ++ nodep.map(_._2), sorted ++ nodep.map(_._1))
    }
}

class LocalAllocator(preservedLocals: Seq[DataLabel])(aliasOf: DataLabel => Set[DataLabel]) {
  private[this] val allocated = mutable.HashMap.empty[DataLabel, Int]
  private[this] var nextLocal = preservedLocals.size

  preservedLocals.zipWithIndex foreach { case (l, i) =>
    aliasOf(l).foreach { a => allocated(a) = i }
  }

  def size: Int = nextLocal
  def apply(l: Some[DataLabel]): Int =
    apply(l.get)
  def apply(l: DataLabel): Int =
    allocated.get(l) getOrElse {
      val local = nextLocal
      aliasOf(l).foreach { a => allocated(a) = local }
      // TODO: support 2word
      nextLocal += 1
      local
    }
}

sealed abstract class TypeRef 
object TypeRef {
  def from(c: Class[_]): Public = {
    if(c == java.lang.Integer.TYPE) Int
    else if(c == java.lang.Long.TYPE) Long
    else if(c == java.lang.Character.TYPE) Char
    else if(c == java.lang.Byte.TYPE) Byte
    else if(c == java.lang.Boolean.TYPE) Boolean
    else if(c == java.lang.Short.TYPE) Short
    else if(c == java.lang.Float.TYPE) Float
    else if(c == java.lang.Double.TYPE) Double
    else if(c == java.lang.Void.TYPE) Void
    else if(c.isArray) ???
    else Reference(ClassName(c.getName))
  }

  case object Undefined extends TypeRef
  case object This extends TypeRef
  case object Null extends TypeRef

  sealed abstract class Public extends TypeRef {
    def str: String
  }

  sealed abstract class Primitive(override val str: String) extends Public

  object Byte extends Primitive("B")
  object Boolean extends Primitive("Z")
  object Char extends Primitive("C")
  object Short extends Primitive("S")
  object Int extends Primitive("I")
  object Float extends Primitive("F")
  object Long extends Primitive("J")
  object Double extends Primitive("D")
  object Void extends Primitive("V")

  case class Reference(className: ClassName) extends Public {
    override def str = s"L${className.binaryString};"
  }
}

case class ClassName(str: String) {
  def binaryString: String = str.replaceAll("\\.", "/")
}
case class MethodDescriptor(ret: TypeRef.Public, args: Seq[TypeRef.Public]) {
  def str: String = s"${args.map(_.str).mkString("(", "", ")")}${ret.str}"
  def isVoid: Boolean = ret == TypeRef.Void
}
object MethodDescriptor {
  def parse(src: String): MethodDescriptor =
    Parser.parse(src)

  def from(m: JMethod): MethodDescriptor =
    MethodDescriptor(TypeRef.from(m.getReturnType), m.getParameterTypes.map(TypeRef.from).toSeq)

  object Parser extends scala.util.parsing.combinator.RegexParsers {
    def parse(src: String): MethodDescriptor =
      parseAll(all, src).get
    lazy val all = args ~ tpe ^^ { case args ~ ret => MethodDescriptor(ret, args) }
    lazy val args = ('(' ~> rep(tpe)) <~ ')'
    val refPat = """L([^;]+);""".r
    lazy val tpe = "B|Z|C|S|I|F|J|D|V|L[^;]+;".r ^^ {
      case "B" => TypeRef.Byte
      case "Z" => TypeRef.Boolean
      case "C" => TypeRef.Char
      case "S" => TypeRef.Short
      case "I" => TypeRef.Int
      case "F" => TypeRef.Float
      case "J" => TypeRef.Long
      case "D" => TypeRef.Double
      case "V" => TypeRef.Void
      case `refPat`(ref) => TypeRef.Reference(ClassName(ref.replaceAll("/", ".")))
    }
  }
}
case class LocalMethodRef(name: String, descriptor: MethodDescriptor) {
  def str: String = name + descriptor.str
  def isVoid: Boolean = descriptor.isVoid
  def args: Seq[TypeRef.Public] = descriptor.args
  def ret: TypeRef.Public = descriptor.ret
}
object LocalMethodRef {
  def from(m: JMethod): LocalMethodRef =
    LocalMethodRef(m.getName, MethodDescriptor.from(m))
  def apply(src: String): LocalMethodRef =
    Parser.parse(src)
  object Parser {
    lazy val all = """([^(]+)(\(.+)""".r
    def parse(src: String): LocalMethodRef =
      src match {
        case `all`(name, desc) =>
          LocalMethodRef(name, MethodDescriptor.parse(desc))
        case unk =>
          throw new IllegalArgumentException(s"Invalid method ref: ${unk}")
      }
  }
}
case class Frame(locals: Seq[DataLabel.Out], stack: List[DataLabel.Out], effect: Effect) {
  def local(n: Int): DataLabel.Out =
    locals(n)

  def dropStack(n: Int): Frame =
    if(stack.length < n) throw new IllegalArgumentException(s"Stack size is ${stack.size}, ${n} required.")
    else Frame(locals, stack.drop(n), effect)

  def pushStack(l: DataLabel.Out): Frame =
    Frame(locals, l +: stack, effect)

  def takeStack(n: Int): List[DataLabel.Out] =
    if(stack.length < n) throw new IllegalArgumentException(s"Stack size is ${stack.size}, ${n} required.")
    else stack.take(n)
}
object Frame {
  val empty = Frame(Seq.empty, List.empty, Effect.fresh)
}

case class Data(typeRef: TypeRef, value: Option[Any])
object Data {
  val Undefined = Data(TypeRef.Undefined, None)
}

case class FrameUpdate(
  newFrame: Frame,
  dataIn: Seq[(DataLabel.Out, DataLabel.In)] = Seq.empty
)

abstract class AbstractLabel() extends AnyRef {
  override def equals(other: Any): Boolean =
    other match {
      case r: AnyRef => this eq r
      case _ => false
    }
  override def hashCode: Int =
    System.identityHashCode(this)
}
object AbstractLabel {
  class Namer[A <: AbstractLabel](idPrefix: String, namePrefix: String) {
    private[this] val ids = mutable.HashMap.empty[A, Int]
    private[this] var nextId = 0

    def num(l: A): Int =
      ids.get(l) getOrElse {
        ids(l) = nextId
        nextId += 1
        ids(l)
      }
    def id(l: A): String = s"${idPrefix}${num(l)}"
    def name(l: A): String = s"${namePrefix}${num(l)}"
  }
  class Assigner[A, L <: AbstractLabel](fresh: =>L) {
    private[this] val mapping = mutable.HashMap.empty[A, L]
    def apply(key: A): L =
      mapping.get(key) getOrElse {
        val l = fresh
        mapping(key) = l
        l
      }
  }
}

final class InstructionLabel private() extends AbstractLabel
object InstructionLabel {
  def fresh(): InstructionLabel = new InstructionLabel
}

final class JumpTarget private extends AbstractLabel
object JumpTarget {
  def fresh(): JumpTarget = new JumpTarget
}

sealed abstract class DataLabel private(val name: String) extends AbstractLabel
object DataLabel {
  final class In (name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.In(${name})@${System.identityHashCode(this)}"
  }
  final class Out(name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.Out(${name})@${System.identityHashCode(this)}"
  }

  def in(name: String) = new In(name)
  def out(name: String) = new Out(name)
}


final class Effect private extends AbstractLabel
object Effect {
  def fresh() = new Effect
}
