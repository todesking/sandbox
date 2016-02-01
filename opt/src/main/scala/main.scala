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

object InstructionSerializer {
  def serialize(body: MethodBody): Seq[Instruction] = {
    Util.tsort(body.instructions)(_.label) { insn => body.dependentInstructions(insn.label) }
  }
}

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

class LocalAllocator(body: MethodBody) {
  private[this] val allocated = mutable.HashMap.empty[DataLabel, Int]
  private[this] var nextLocal = body.initialFrame.locals.size

  body.initialFrame.locals.zipWithIndex foreach { case (l, i) =>
    body.dataAliases(l).foreach { a => allocated(a) = i }
  }

  def size: Int = nextLocal
  def apply(l: Some[DataLabel]): Int =
    apply(l.get)
  def apply(l: DataLabel): Int =
    allocated.get(l) getOrElse {
      val local = nextLocal
      body.dataAliases(l).foreach { a => allocated(a) = local }
      // TODO: support 2word
      nextLocal += 1
      local
    }
}

case class MethodBodyBytecode(instructions: Seq[AbstractBytecode], maxLocals: Int, maxStackDepth: Int)
sealed abstract class AbstractBytecode {
  val label: AbstractBytecode.Label = AbstractBytecode.Label.fresh()
}
object AbstractBytecode {
  class Label extends AbstractLabel
  object Label {
    def fresh(): Label = new Label
  }

  def load(t: TypeRef, n: Int): AbstractBytecode =
    t match {
      case TypeRef.Int =>iload(n)
      case TypeRef.Reference(_) => aload(n)
      case unk =>
        throw new IllegalArgumentException(s"Unsupported load instruction for ${unk}")
    }

  def store(t: TypeRef, n: Int): AbstractBytecode =
    t match {
      case TypeRef.Int => istore(n)
      case unk =>
        throw new IllegalArgumentException(s"Unsupported store instruction for ${unk}")
    }

  case class iload(n: Int) extends AbstractBytecode
  case class aload(n: Int) extends AbstractBytecode
  case class istore(n: Int) extends AbstractBytecode
  case class ireturn() extends AbstractBytecode
  case class iconst(value: Int) extends AbstractBytecode
  case class goto(target: JumpTarget) extends AbstractBytecode
  case class if_icmple(target: JumpTarget) extends AbstractBytecode
  case class invokevirtual(className: ClassName, methodRef: LocalMethodRef) extends AbstractBytecode
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

// NOTE: Equality is defined without label
sealed abstract class Instruction {
  final val label = InstructionLabel.fresh()
  def nextFrame(frame: Frame): FrameUpdate
  def updateValues(f: DataLabel.In => Data): Map[DataLabel.Out, Data]
  val output: Option[DataLabel.Out]
  val inputs: Seq[DataLabel.In]
}
object Instruction {
  case class Return() extends Instruction {
    val retVal: DataLabel.In = DataLabel.in("retval")
    override val inputs = Seq(retVal)
    override val output = None
    override def nextFrame(frame: Frame) =
      FrameUpdate(Frame.empty, Seq(frame.stack(0) -> retVal))
    override def updateValues(f: DataLabel.In => Data) = Map.empty
  }

  case class Const(tpe: TypeRef, value: Any) extends Instruction {
    val valueLabel = DataLabel.out("const")
    override val output = Some(valueLabel)
    override val inputs = Seq.empty
    override def nextFrame(frame: Frame) =
      FrameUpdate(frame.pushStack(valueLabel))
    override def updateValues(f: DataLabel.In => Data) =
      Map(valueLabel -> Data(tpe, Some(value)))
  }

  case class InvokeVirtual(className: ClassName, methodRef: LocalMethodRef) extends Instruction {
    override val inputs = (0 to methodRef.descriptor.args.size).map { i => DataLabel.in(if(i == 0) "receiver" else s"arg_${i}") }.toSeq
    override val output = if(methodRef.isVoid) None else Some(DataLabel.out("ret"))
    val argAndType: Seq[(DataLabel.In, TypeRef.Public)] = inputs.zip(TypeRef.Reference(className) +: methodRef.descriptor.args)
    override def nextFrame(frame: Frame) =
      FrameUpdate(
        output.map { o =>
          frame.dropStack(methodRef.descriptor.args.size + 1).pushStack(o)
        } getOrElse {
          frame.dropStack(methodRef.descriptor.args.size + 1)
        },
        dataIn = frame.takeStack(methodRef.descriptor.args.size + 1).reverse.zip(inputs)
      )
    override def updateValues(f: DataLabel.In => Data) =
      output.map(_ -> Data(methodRef.descriptor.ret, None)).toMap
  }

  case class IfICmpLE(thenTarget: JumpTarget, elseTarget: JumpTarget) extends Instruction {
    val value1 =DataLabel.in("value1")
    val value2 =DataLabel.in("value2")
    override val output = None
    override val inputs = Seq(value2, value1)
    override def nextFrame(frame: Frame) =
      FrameUpdate(
        frame.dropStack(2),
        frame.takeStack(2).zip(inputs)
      )
    override def updateValues(f: DataLabel.In => Data) =
      Map.empty
  }

  case class Goto(target: JumpTarget) extends Instruction {
    override val output = None
    override val inputs = Seq.empty
    override def nextFrame(frame: Frame) =
      FrameUpdate(frame)
    override def updateValues(f: DataLabel.In => Data) =
      Map.empty
  }
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
