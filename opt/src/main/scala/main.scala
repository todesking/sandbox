package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.immutable.Stack

import java.lang.reflect.{Method => JMethod}

object Main {
  def main(args: Array[String]): Unit = {
  }
}

object Opt {
  def optimize[A: ClassTag](orig: A): A = {
    orig
  }
}

class UnsupportedOpcodeException(byte: Int)
  extends RuntimeException(f"Unsupported opcode: 0x$byte%02X")

class AnalyzeException(msg: String) extends RuntimeException(msg)

sealed abstract class Instance[A] {
  def methods: Set[LocalMethodRef]

  def hasMethod(name: String, descriptor: String): Boolean =
    hasMethod(name, MethodDescriptor.parse(descriptor))

  def hasMethod(name: String, descriptor: MethodDescriptor): Boolean =
    hasMethod(LocalMethodRef(name, descriptor))

  def hasMethod(ref: LocalMethodRef): Boolean =
    methods.contains(ref)

  def methodBody(ref: LocalMethodRef): Option[MethodBody]
}
object Instance {
  case class Native[A <: AnyRef](value: A) extends Instance[A] {
    private[this] val javaClass = value.getClass

    private[this] def javaMethod(ref: LocalMethodRef): Option[JMethod] =
      javaClass.getMethods.find { m => LocalMethodRef.from(m) == ref }

    override val methods =
      javaClass.getMethods.map(LocalMethodRef.from).toSet

    override def methodBody(ref: LocalMethodRef): Option[MethodBody] =
      javaMethod(ref).flatMap(MethodBody.parse(value, _))
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
  object Double extends Primitive("V")
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
}
object MethodDescriptor {
  def parse(src: String): MethodDescriptor = {
    if(src == "()I") MethodDescriptor(TypeRef.Int, Seq.empty)
    else ???
  }
  def from(m: JMethod): MethodDescriptor =
    MethodDescriptor(TypeRef.from(m.getReturnType), m.getParameterTypes.map(TypeRef.from).toSeq)
}
case class LocalMethodRef(name: String, descriptor: MethodDescriptor) {
  def str: String = name + descriptor.str
}
object LocalMethodRef {
  def from(m: JMethod): LocalMethodRef =
    LocalMethodRef(m.getName, MethodDescriptor.from(m))
}

case class MethodBody(
  initialFrame: Frame,
  returns: Seq[Instruction.Return],
  dataValues: Map[DataLabel, Data],
  dataFlow: Seq[(DataLabel.Out, DataLabel.In)],
  dataSources: Map[DataLabel.Out, Instruction]
) {
  def data(label: DataLabel): Data =
    dataValues.get(label) getOrElse {
      throw new IllegalArgumentException("data not found") }

  def dataSource(l: DataLabel): Option[Instruction] = l match {
    case l: DataLabel.In =>
      dataFlow.find { case (o, i) => i == l }.map(_._1).flatMap(dataSource)
    case l: DataLabel.Out =>
      dataSources.get(l)
  }
}
object MethodBody {
  def parse(instance: AnyRef, m: JMethod): Option[MethodBody] = {
    require(instance != null)
    require(m != null)
    import scala.collection.mutable

    val jClass = instance.getClass
    import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod }
    val classPool = new ClassPool(null)
    classPool.appendClassPath(new ClassClassPath(jClass))

    val ctClass = classPool.get(jClass.getName)
    val mRef = LocalMethodRef.from(m)

    val ctMethod = ctClass.getMethod(mRef.name, mRef.descriptor.str)

    if(ctMethod.isEmpty) { // "abstract or native"(from CtClass doc)
      None
    } else {
      val thisData = (DataLabel.out() -> Data(TypeRef.This, Some(instance)))
      val argData = m.getParameterTypes.map { t =>
        (DataLabel.out() -> Data(TypeRef.from(t), None))
      }
      val initialFrame = Frame(thisData._1 +: argData.map(_._1), Stack.empty)
      val insns = mutable.ArrayBuffer.empty[Instruction]
      val exchanges = mutable.HashMap.empty[InstructionLabel, Frame => Frame]
      val index2ILabel = mutable.HashMap.empty[Int, InstructionLabel]
      val dataFlow = mutable.ArrayBuffer.empty[(DataLabel.Out, DataLabel.In)]
      val dataValues = mutable.HashMap.empty[DataLabel, Data]
      val dataSources = mutable.HashMap.empty[DataLabel.Out, Instruction]

      dataValues(thisData._1) = thisData._2
      argData foreach { case (l, d) => dataValues(l) = d }

      // 2 is read-only version of getMethodInfo
      val it = ctMethod.getMethodInfo2.getCodeAttribute.iterator
      it.begin()
      while(it.hasNext) {
        val index = it.next()
        it.byteAt(index) match {
          case 0x00 => // nop
          case 0x01 => // aconst_null
            insns += Instruction.Const(null, TypeRef.Null)
          // TODO
          case 0x03 => // iconst_0
            insns += Instruction.Const(0, TypeRef.Int)
          case 0x04 => // iconst_1
            insns += Instruction.Const(1, TypeRef.Int)
          case 0x05 => // iconst_2
            insns += Instruction.Const(2, TypeRef.Int)
          case 0x06 => // iconst_3
            insns += Instruction.Const(3, TypeRef.Int)
          case 0x07 => // iconst_4
            insns += Instruction.Const(4, TypeRef.Int)
          case 0x08 => // iconst_5
            insns += Instruction.Const(5, TypeRef.Int)
          // TODO
          case 0xAC => // ireturn
            insns += Instruction.Return()
          case unk => throw new UnsupportedOpcodeException(unk)
        }
      }

      insns foreach { insn =>
        insn.output foreach { out =>
          dataSources += (out -> insn)
        }
      }

      var currentFrame = initialFrame
      insns foreach { insn =>
        val update = insn.nextFrame(currentFrame)
        update.dataIn foreach { dataFlow += _ }
        update.dataValues foreach { dataValues += _ }
        currentFrame = update.newFrame
      }

      dataFlow foreach { case(o, i) =>
        (dataValues.get(o), dataValues.get(i)) match {
          case (Some(vo), None) =>
            dataValues(i) = vo
          case (None, Some(vi)) =>
            dataValues(o) = vi
          case (Some(vo), Some(vi)) if vi == vo =>
            // nothing to do
          case (Some(vo), Some(vi)) =>
            throw new AnalyzeException(s"Data value conflict: ${vo}, ${vi}")
          case (None, None) =>
            throw new AnalyzeException("Undefined data value found")
        }
      }

      Some(MethodBody(
        initialFrame,
        insns.collect { case r @ Instruction.Return() => r }.toSeq,
        dataValues.toMap,
        dataFlow,
        dataSources.toMap
      ))
    }
  }
}

case class Frame(locals: Seq[DataLabel.Out], stack: Stack[DataLabel.Out]) {
  def local(n: Int): DataLabel.Out =
    locals(n)

  def dropStack(n: Int): Frame =
    Frame(locals, stack.drop(n))

  def pushStack(l: DataLabel.Out): Frame =
    Frame(locals, stack.push(l))
}
object Frame {
  val empty = Frame(Seq.empty, Stack.empty)
}

case class Data(typeRef: TypeRef, value: Option[Any])
object Data {
  val Undefined = Data(TypeRef.Undefined, None)
}

// NOTE: Equality is defined without label
sealed abstract class Instruction {
  final val label = InstructionLabel.fresh()
  def nextFrame(frame: Frame): FrameUpdate
  def output: Option[DataLabel.Out]
}
object Instruction {
  case class Return() extends Instruction {
    val retVal: DataLabel.In = DataLabel.in()
    override def output = None
    override def nextFrame(frame: Frame) =
      FrameUpdate(Frame.empty, Seq(frame.stack(0) -> retVal))
  }

  case class Const(value: Any, tpe: TypeRef) extends Instruction {
    val valueLabel = DataLabel.out()
    override def output = Some(valueLabel)
    override def nextFrame(frame: Frame) =
      FrameUpdate(
        frame.pushStack(valueLabel),
        dataOut = Seq(valueLabel),
        dataValues = Seq(valueLabel -> Data(tpe, Some(value))))
  }
}

case class FrameUpdate(
  newFrame: Frame,
  dataIn: Seq[(DataLabel.Out, DataLabel.In)] = Seq.empty,
  dataOut: Seq[DataLabel.Out] = Seq.empty,
  dataValues: Seq[(DataLabel, Data)] = Seq.empty
)

abstract class AbstractLabel extends AnyRef {
  override def equals(other: Any): Boolean =
    super.equals(other)
  override def hashCode: Int =
    super.hashCode()
}

final class InstructionLabel private() extends AbstractLabel
object InstructionLabel {
  def fresh(): InstructionLabel = new InstructionLabel
}

sealed abstract class DataLabel private() extends AbstractLabel
object DataLabel {
  final class In () extends DataLabel
  final class Out() extends DataLabel

  def in() = new In()
  def out() = new Out()
}

