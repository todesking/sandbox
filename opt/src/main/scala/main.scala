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

sealed abstract class Instance[A <: AnyRef : ClassTag] {
  def methods: Set[LocalMethodRef]

  def hasMethod(name: String, descriptor: String): Boolean =
    hasMethod(name, MethodDescriptor.parse(descriptor))

  def hasMethod(name: String, descriptor: MethodDescriptor): Boolean =
    hasMethod(LocalMethodRef(name, descriptor))

  def hasMethod(ref: LocalMethodRef): Boolean =
    methods.contains(ref)

  def methodBody(ref: LocalMethodRef): Option[MethodBody]

  def instance(): A

  def replaceInstruction(ref: LocalMethodRef, target: InstructionLabel, instruction: Instruction): Instance[A] = {
    val newBody =
      methodBody(ref).map { b =>
        b.replace(target, instruction)
      } getOrElse { throw new IllegalArgumentException(s"Can't rewrite ${ref.str}: Method body inaccessible") }
    Instance.Rewritten[A](this, Map(ref -> newBody))
  }
}
object Instance {
  case class Native[A <: AnyRef : ClassTag](value: A) extends Instance[A] {
    private[this] val javaClass = value.getClass

    private[this] def javaMethod(ref: LocalMethodRef): Option[JMethod] =
      javaClass.getMethods.find { m => LocalMethodRef.from(m) == ref }

    private[this] val methodBodies = mutable.HashMap.empty[LocalMethodRef, Option[MethodBody]]

    override val methods =
      javaClass.getMethods.map(LocalMethodRef.from).toSet

    override def methodBody(ref: LocalMethodRef): Option[MethodBody] = synchronized {
      methodBodies.get(ref) getOrElse {
        val body = javaMethod(ref).flatMap(MethodBody.parse(value, _))
        methodBodies(ref) = body
        body
      }
    }

    override def instance(): A =
      value

  }

  case class Rewritten[A <: AnyRef : ClassTag](
    base: Instance[A],
    methodBodies: Map[LocalMethodRef, MethodBody] = Map.empty
  ) extends Instance[A] {
    override def methods = base.methods
    override def methodBody(ref: LocalMethodRef) = methodBodies.get(ref) orElse base.methodBody(ref)
    override def instance() = {
      // create newClass < class[A]
      // for each public/protected methods of A
      //   if the method has no body(i.e native/abstract) => do nothing
      //   if the method is rewritten
      //     add the method to newClass
      //   if the method is overriden
      //     add the method to newClass
      //   if the method is added and it references other method in original instance, and it is declared in subclass of A
      //     add the referenced method, and do recursively
      // TODO: fields
      // return new instance
      ???
    }
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
}
object MethodDescriptor {
  def parse(src: String): MethodDescriptor = {
    if(src == "()I") MethodDescriptor(TypeRef.Int, Seq.empty)
    else if(src == "()D") MethodDescriptor(TypeRef.Double, Seq.empty)
    else if(src == "()J") MethodDescriptor(TypeRef.Long, Seq.empty)
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
  isStatic: Boolean,
  descriptor: MethodDescriptor,
  initialFrame: Frame,
  firstInstruction: InstructionLabel,
  dataFlow: Seq[(DataLabel.Out, DataLabel.In)],
  instructions: Seq[Instruction]
) {
  private[this] lazy val dataValues: Map[DataLabel, Data] = {
    val m = mutable.HashMap.empty[DataLabel, Data]
    val tasks = mutable.Set.empty[(InstructionLabel, Frame)]
    tasks += (firstInstruction -> initialFrame)
    if(isStatic) {
      descriptor.args.zip(initialFrame.locals) foreach { case (tpe, l) => m += (l -> Data(tpe, None)) }
    } else {
      m += (initialFrame.locals(0) -> Data(TypeRef.This, None))
      descriptor.args.zip(initialFrame.locals.tail) foreach { case (tpe, l) => m += (l -> Data(tpe, None)) }
    }
    while(tasks.nonEmpty) {
      val (i, f) = tasks.head
      tasks -= (i -> f)
      val insn = instruction(i)
      val update = insn.nextFrame(f)
      update.dataValues foreach { case (out, d) =>
        m += (out -> d)
        (out -> d).pp()
        outData2InData(out) foreach { in =>
          (in -> d).pp()
          m += (in -> d)
        }
      }
      dependerInstructions(insn.label) foreach { d =>
        tasks += (d.label -> update.newFrame)
      }
    }
    m.toMap
  }

  private[this] lazy val label2Insn =
    instructions.map { i => (i.label -> i) }.toMap

  private[this] lazy val inData2Insn =
    instructions.flatMap { i => i.inputs.map { ii => (ii -> i) } }.toMap

  private[this] lazy val outData2InData: Map[DataLabel.Out, Set[DataLabel.In]] =
    dataFlow.groupBy(_._1).map { case (o, ois) => (o -> ois.map(_._2).toSet) }.toMap

  private[this] def instruction(label: InstructionLabel): Instruction =
    label2Insn.get(label) getOrElse { throw new IllegalArgumentException(s"Instruction ${label} not found") }

  def dependerInstructions(label: InstructionLabel): Seq[Instruction] =
    instruction(label).output.toSeq.flatMap { o =>
      dataFlow.filter(_._1 == o).map(_._2).map(inData2Insn)
    }

  lazy val returns: Seq[Instruction.Return] = instructions.collect { case r @ Instruction.Return() => r }

  def data(label: DataLabel): Data =
    dataValues.get(label) getOrElse {
      throw new IllegalArgumentException(s"data not found: ${label}") }

  def dataSource(l: DataLabel): Option[Instruction] = l match {
    case l: DataLabel.In =>
      dataFlow.find { case (o, i) => i == l }.map(_._1).flatMap(dataSource)
    case l: DataLabel.Out =>
      instructions.find(_.output == Some(l))
  }

  def replace(target: InstructionLabel, replace: Instruction): MethodBody = {
    instructions.map(_.label).pp()
    val targetInstruction = instructions.find(_.label == target).getOrElse {
      throw new IllegalArgumentException(s"Replace target instruction not found: ${target}")
    }

    require(targetInstruction.inputs.size == replace.inputs.size)
    require(targetInstruction.output.size == replace.output.size)

    val newDataFlow = dataFlow.map {
      case (out, in) if targetInstruction.output == Some(out) =>
        (replace.output.get -> in)
      case (out, in) if targetInstruction.inputs.contains(in) =>
        (out -> replace.inputs.zip(targetInstruction.inputs).find(_._2 == in).get._1)
      case (out, in) =>
        (out -> in)
    }
    val newInstructions = instructions.filterNot(_.label == target) :+ replace
    MethodBody(
      isStatic,
      descriptor,
      initialFrame,
      if(firstInstruction == target) replace.label
      else firstInstruction,
      newDataFlow,
      instructions.filterNot(_.label == target) :+ replace
    )
  }
}
object MethodBody {
  def parse(instance: AnyRef, m: JMethod): Option[MethodBody] = {
    require(instance != null)
    require(m != null)

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
      val isStatic = (ctMethod.getMethodInfo2.getAccessFlags & 0x08) == 0x08
      val argLabels = mRef.descriptor.args.map { _ => DataLabel.out() }
      val initialFrame =
        if(isStatic) Frame(argLabels, List.empty)
        else Frame(DataLabel.out() +: argLabels, List.empty)
      val insns = mutable.ArrayBuffer.empty[Instruction]
      val exchanges = mutable.HashMap.empty[InstructionLabel, Frame => Frame]
      val index2ILabel = mutable.HashMap.empty[Int, InstructionLabel]
      val dataFlow = mutable.ArrayBuffer.empty[(DataLabel.Out, DataLabel.In)]
      val dataSources = mutable.HashMap.empty[DataLabel.Out, Instruction]

      // TODO: supprot 2 word data

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
            insns += Instruction.Const(TypeRef.Int, 0)
          case 0x04 => // iconst_1
            insns += Instruction.Const(TypeRef.Int, 1)
          case 0x05 => // iconst_2
            insns += Instruction.Const(TypeRef.Int, 2)
          case 0x06 => // iconst_3
            insns += Instruction.Const(TypeRef.Int, 3)
          case 0x07 => // iconst_4
            insns += Instruction.Const(TypeRef.Int, 4)
          case 0x08 => // iconst_5
            insns += Instruction.Const(TypeRef.Int, 5)
          case 0x09 => // lconst_0
            insns += Instruction.Const(TypeRef.Long, 0L)
          // TODO
          case 0xAC => // ireturn
            insns += Instruction.Return()
          // TODO
          case 0xAD => // lreturn
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
        currentFrame = update.newFrame
      }

      Some(MethodBody(
        isStatic,
        mRef.descriptor,
        initialFrame,
        insns.head.label,
        dataFlow,
        insns
      ))
    }
  }
}

case class Frame(locals: Seq[DataLabel.Out], stack: List[DataLabel.Out]) {
  def local(n: Int): DataLabel.Out =
    locals(n)

  def dropStack(n: Int): Frame =
    Frame(locals, stack.drop(n))

  def pushStack(l: DataLabel.Out): Frame =
    Frame(locals, l +: stack)
}
object Frame {
  val empty = Frame(Seq.empty, List.empty)
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
  def inputs: Seq[DataLabel.In]
}
object Instruction {
  case class Return() extends Instruction {
    val retVal: DataLabel.In = DataLabel.in()
    override def inputs = Seq(retVal)
    override def output = None
    override def nextFrame(frame: Frame) =
      FrameUpdate(Frame.empty, Seq(frame.stack(0) -> retVal))
  }

  case class Const(tpe: TypeRef, value: Any) extends Instruction {
    val valueLabel = DataLabel.out()
    override def output = Some(valueLabel)
    override def inputs = Seq.empty
    override def nextFrame(frame: Frame) =
      FrameUpdate(
        frame.pushStack(valueLabel),
        dataValues = Seq(valueLabel -> Data(tpe, Some(value))))
  }
}

case class FrameUpdate(
  newFrame: Frame,
  dataIn: Seq[(DataLabel.Out, DataLabel.In)] = Seq.empty,
  dataValues: Seq[(DataLabel.Out, Data)] = Seq.empty
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

final class JumpTarget private() extends AbstractLabel
object JumpTarget {
  def fresh(): JumpTarget = new JumpTarget
}

sealed abstract class DataLabel private() extends AbstractLabel
object DataLabel {
  final class In () extends DataLabel
  final class Out() extends DataLabel

  def in() = new In()
  def out() = new Out()
}

