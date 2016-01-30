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

  def methodModified(ref: LocalMethodRef): Boolean

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

    override def methodModified(ref: LocalMethodRef) = false

  }

  case class Rewritten[A <: AnyRef : ClassTag](
    base: Instance[A],
    methodBodies: Map[LocalMethodRef, MethodBody] = Map.empty
  ) extends Instance[A] {
    override def methods = base.methods
    override def methodBody(ref: LocalMethodRef) =
      methodBodies.get(ref) orElse base.methodBody(ref)
    override def methodModified(ref: LocalMethodRef) =
      methodBodies.get(ref).map(_ => true) getOrElse base.methodModified(ref)
    override def instance() = {
      import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod}
      import javassist.bytecode.{Bytecode, MethodInfo}

      val jClass = classTag[A].runtimeClass

      val classPool = new ClassPool(null)
      classPool.appendClassPath(new ClassClassPath(jClass))

      val baseClass = classPool.get(jClass.getName)
      // TODO make name unique
      val klass = classPool.makeClass(jClass.getName + "_rewritten", baseClass)
      val constPool = klass.getClassFile.getConstPool

      def ctClass(tr: TypeRef): CtClass = {
        tr match {
          case TypeRef.Int => CtClass.intType
          case unk => throw new NotImplementedError(s"${unk}")
        }
      }

      methods.filter(methodModified) foreach { ref =>
        methodBody(ref) foreach { body =>
          val locals = new LocalAllocator(body)
          var stackDepth = 0
          var maxStackDepth = 0
          val bc = new Bytecode(constPool, 0, 0)
          InstructionSerializer.serialize(body) foreach { insn =>
            insn match {
              case i @ Instruction.Return() =>
                val tr = body.data(i.retVal).typeRef
                if(tr == TypeRef.Int) {
                  bc.addIload(locals(i.retVal))
                } else {
                  throw new NotImplementedError(tr.toString)
                }
                bc.addReturn(ctClass(tr))
              case i @ Instruction.Const(typeRef, value) =>
                if(typeRef == TypeRef.Int) {
                  bc.addIconst(value.asInstanceOf[Int])
                  bc.addIstore(locals(i.valueLabel))
                } else throw new NotImplementedError(typeRef.toString)
                stackDepth += 1
              case i @ Instruction.InvokeVirtual(className, methodRef) =>
                i.inputs.foreach { in =>
                  body.data(in).typeRef match {
                    case TypeRef.This =>
                      bc.addAload(locals(in))
                      stackDepth += 1
                    case TypeRef.Int =>
                      bc.addIload(locals(in))
                      stackDepth += 1
                    case unk => throw new NotImplementedError(unk.toString)
                  }
                }
                maxStackDepth = Math.max(stackDepth, maxStackDepth)
                bc.addInvokevirtual(className.str, methodRef.name, methodRef.descriptor.str)
                stackDepth -= methodRef.descriptor.args.size + 1
                if(methodRef.descriptor.ret != TypeRef.Void) {
                  stackDepth += 1
                  if(methodRef.descriptor.ret == TypeRef.Int) {
                    bc.addIstore(locals(i.output.get))
                  } else {
                    throw new NotImplementedError(methodRef.descriptor.ret.toString)
                  }
                }
            }
            maxStackDepth = Math.max(stackDepth, maxStackDepth)
          }
          bc.setMaxLocals(locals.size)
          bc.setMaxStack(maxStackDepth)
          val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
          minfo.setCodeAttribute(bc.toCodeAttribute)
          klass.getClassFile.addMethod(minfo)
        }
      }
      // TODO: fields

      klass.toClass(new java.net.URLClassLoader(Array.empty, jClass.getClassLoader), null).newInstance().asInstanceOf[A]
    }
  }
}

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
  def apply(l: DataLabel): Int =
    allocated.get(l) getOrElse {
      val local = nextLocal
      body.dataAliases(l).foreach { a => allocated(a) = local }
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
    if(isStatic) {
      descriptor.args.zip(initialFrame.locals) foreach { case (tpe, l) => m += (l -> Data(tpe, None)) }
    } else {
      m += (initialFrame.locals(0) -> Data(TypeRef.This, None))
      descriptor.args.zip(initialFrame.locals.tail) foreach { case (tpe, l) => m += (l -> Data(tpe, None)) }
    }
    initialFrame.locals foreach { l =>
      outData2InData.get(l) foreach { ins =>
        ins foreach { in =>
          m += (in -> m(l))
        }
      }
    }
    val in2out = dataFlow.map { case (a, b) => (b -> a) }.toMap
    InstructionSerializer.serialize(this) foreach { insn =>
      insn.inputs foreach { in => m(in) = m(in2out(in)) }
      insn.updateValues(m) foreach { case (k, v) => m(k) = v }
    }
    m.toMap
  }

  private[this] lazy val label2Insn =
    instructions.map { i => (i.label -> i) }.toMap

  private[this] lazy val inData2Insn =
    instructions.flatMap { i => i.inputs.map { ii => (ii -> i) } }.toMap

  private[this] lazy val outData2InData: Map[DataLabel.Out, Set[DataLabel.In]] =
    dataFlow.groupBy(_._1).map { case (o, ois) => (o -> ois.map(_._2).toSet) }.toMap

  private[this] lazy val outData2Insn =
    instructions.flatMap { i => i.output.map { out => (out -> i) } }.toMap

  def dataAliases(l: DataLabel): Set[DataLabel] =
    l match {
      case l: DataLabel.In => dataFlow.filter(_._2 == l).map(_._1).toSet + l
      case l: DataLabel.Out => dataFlow.filter(_._1 == l).map(_._2).toSet + l
    }

  def instruction(label: InstructionLabel): Instruction =
    label2Insn.get(label) getOrElse { throw new IllegalArgumentException(s"Instruction ${label} not found") }

  def dependerInstructions(label: InstructionLabel): Set[InstructionLabel] =
    instruction(label).output.toSeq.flatMap { o =>
      dataFlow.filter(_._1 == o).map(_._2).map(inData2Insn).map(_.label)
    }.toSet

  def dependentInstructions(label: InstructionLabel): Set[InstructionLabel] =
    instruction(label).inputs.flatMap { in =>
      dataFlow.filter(_._2 == in).map(_._1).map(outData2Insn.get).flatten.map(_.label)
    }.toSet

  lazy val returns: Seq[Instruction.Return] = instructions.collect { case r @ Instruction.Return() => r }

  def data(label: DataLabel): Data =
    dataValues.get(label) getOrElse {
      throw new IllegalArgumentException(s"data not found: ${label}, registered=${dataFlow.flatMap { case (a, b) => Seq(a, b) }}") }

  def dataSource(l: DataLabel): Option[Instruction] = l match {
    case l: DataLabel.In =>
      dataFlow.find { case (o, i) => i == l }.map(_._1).flatMap(dataSource)
    case l: DataLabel.Out =>
      instructions.find(_.output == Some(l))
  }

  def replace(target: InstructionLabel, replace: Instruction): MethodBody = {
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

  def toDot(): String = {
    val dataNames = new AbstractLabel.Namer[DataLabel]("data_", "data #")
    val insnNames = new AbstractLabel.Namer[InstructionLabel]("insn_", "#")
    s"""digraph {
${
  instructions.map { insn =>
    s"""${insnNames.id(insn.label)}[label="${insn}"];"""
  }.mkString("\n")
}
${
  instructions.flatMap { insn =>
    (insn.inputs ++ insn.output).map { i =>
      s"""${dataNames.id(i)}[label="${i.name}: ${try { data(i) } catch { case e => "BUG: Not found" }}"];"""
    }
  }.mkString("\n")
}
${
  initialFrame.locals.distinct.map { o =>
    s"""${dataNames.id(o)}[label="${o.name}: ${try { data(o) } catch { case e => "BUG: Not found" }}"];"""
  }.mkString("\n")
}

${
  instructions.flatMap { insn =>
    insn.inputs.map { in =>
      s"${insnNames.id(insn.label)} -> ${dataNames.id(in)};"
    } ++ insn.output.map { out =>
      s"${dataNames.id(out)} -> ${insnNames.id(insn.label)};"
    }
  }.mkString("\n")
}

${
  dataFlow.map { case (out, in) =>
    s"${dataNames.id(in)} -> ${dataNames.id(out)};"
  }.mkString("\n")
}
}"""
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
    } else if(ctMethod.getMethodInfo2.getCodeAttribute == null) { // ??? but it happens
      None
    } else {
      val isStatic = (ctMethod.getMethodInfo2.getAccessFlags & 0x08) == 0x08
      val argLabels = mRef.descriptor.args.zipWithIndex.map { case (_, i) => DataLabel.out(s"arg_${i + 1}") }
      val initialFrame =
        if(isStatic) Frame(argLabels, List.empty)
        else Frame(DataLabel.out("this") +: argLabels, List.empty)
      val insns = mutable.ArrayBuffer.empty[Instruction]
      val index2ILabel = mutable.HashMap.empty[Int, InstructionLabel]
      val dataFlow = mutable.ArrayBuffer.empty[(DataLabel.Out, DataLabel.In)]

      val ops = mutable.HashMap.empty[JumpTarget, Frame => FrameUpdate]
      val controls = mutable.HashMap.empty[JumpTarget, JumpTarget]
      var prevTarget: JumpTarget = null

      val addr2jumpTargets = mutable.HashMap.empty[Int, JumpTarget]
      def jumpTargetFromAddr(addr: Int): JumpTarget =
        addr2jumpTargets.get(addr).getOrElse {
          val j = JumpTarget.fresh()
          addr2jumpTargets(addr) = j
          j
        }

      def onExchange(addr: Int, name: String)(f: Frame => Frame): Unit = {
        println(s"exchange ${name}")
        val jt = jumpTargetFromAddr(addr)
        ops(jt) = {frame: Frame => FrameUpdate(f(frame)) }
        if(prevTarget != null) controls(prevTarget) = jt
        prevTarget = jt
      }

      def onInstruction(addr: Int, i: Instruction): Unit = {
        println(s"instruction ${i}")
        insns += i
        val jt = jumpTargetFromAddr(addr)
        ops(jt) = { f: Frame => println("Do " + i); i.nextFrame(f) }
        if(prevTarget != null) controls(prevTarget) = jt
        prevTarget = jt
      }


      // TODO: supprot 2 word data

      // 2 is read-only version of getMethodInfo
      val it = ctMethod.getMethodInfo2.getCodeAttribute.iterator
      val cpool = ctClass.getClassFile2().getConstPool()
      it.begin()
      while(it.hasNext) {
        val index = it.next()
        it.byteAt(index) match {
          case 0x00 => // nop
          case 0x01 => // aconst_null
            onInstruction(index, Instruction.Const(null, TypeRef.Null))
          // TODO
          case 0x03 => // iconst_0
            onInstruction(index, Instruction.Const(TypeRef.Int, 0))
          case 0x04 => // iconst_1
            onInstruction(index, Instruction.Const(TypeRef.Int, 1))
          case 0x05 => // iconst_2
            onInstruction(index, Instruction.Const(TypeRef.Int, 2))
          case 0x06 => // iconst_3
            onInstruction(index, Instruction.Const(TypeRef.Int, 3))
          case 0x07 => // iconst_4
            onInstruction(index, Instruction.Const(TypeRef.Int, 4))
          case 0x08 => // iconst_5
            onInstruction(index, Instruction.Const(TypeRef.Int, 5))
          case 0x09 => // lconst_0
            onInstruction(index, Instruction.Const(TypeRef.Long, 0L))
          // TODO
          case 0x2A => // aload_0
            onExchange(index, "aload_0") { frame =>
              frame.pushStack(frame.locals(0))
            }
            // TODO
          case 0xAC => // ireturn
            onInstruction(index, Instruction.Return())
          // TODO
          case 0xAD => // lreturn
            onInstruction(index, Instruction.Return())
          // TODO
          case 0xB6 => // invokevirtual
            val constIndex = it.u16bitAt(index+1)
            val className = cpool.getMethodrefClassName(constIndex)
            val methodName = cpool.getMethodrefName(constIndex)
            val methodType = cpool.getMethodrefType(constIndex)
            onInstruction(index, Instruction.InvokeVirtual(ClassName(className), LocalMethodRef(methodName, MethodDescriptor.parse(methodType))))
          case unk => throw new UnsupportedOpcodeException(unk)
        }
      }

      var currentFrame = initialFrame

      var next: Option[JumpTarget] = Some(jumpTargetFromAddr(0))
      while(next.nonEmpty) {
        println(currentFrame)
        val update = ops(next.get).apply(currentFrame)
        update.dataIn foreach { dataFlow += _ }
        currentFrame = update.newFrame
        next = controls.get(next.get)
      }
      dataFlow.pp()

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
    if(stack.length < n) throw new IllegalArgumentException(s"Stack size is ${stack.size}, ${n} required.")
    else Frame(locals, stack.drop(n))

  def pushStack(l: DataLabel.Out): Frame =
    Frame(locals, l +: stack)

  def takeStack(n: Int): List[DataLabel.Out] =
    if(stack.length < n) throw new IllegalArgumentException(s"Stack size is ${stack.size}, ${n} required.")
    else stack.take(n)
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
  def updateValues(f: DataLabel.In => Data): Map[DataLabel.Out, Data]
  def output: Option[DataLabel.Out]
  def inputs: Seq[DataLabel.In]
}
object Instruction {
  case class Return() extends Instruction {
    val retVal: DataLabel.In = DataLabel.in("retval")
    override def inputs = Seq(retVal)
    override def output = None
    override def nextFrame(frame: Frame) =
      FrameUpdate(Frame.empty, Seq(frame.stack(0) -> retVal))
    override def updateValues(f: DataLabel.In => Data) = Map.empty
  }

  case class Const(tpe: TypeRef, value: Any) extends Instruction {
    val valueLabel = DataLabel.out("const")
    override def output = Some(valueLabel)
    override def inputs = Seq.empty
    override def nextFrame(frame: Frame) =
      FrameUpdate(
        frame.pushStack(valueLabel),
        dataValues = Seq(valueLabel -> Data(tpe, Some(value))))
    override def updateValues(f: DataLabel.In => Data) =
      Map(valueLabel -> Data(tpe, Some(value)))
  }

  case class InvokeVirtual(className: ClassName, methodRef: LocalMethodRef) extends Instruction {
    override val inputs = (0 to methodRef.descriptor.args.size).map { i => DataLabel.in(if(i == 0) "receiver" else s"arg_${i}") }.toSeq
    override val output = if(methodRef.descriptor.ret == TypeRef.Void) None else Some(DataLabel.out("ret"))
    override def nextFrame(frame: Frame) =
      FrameUpdate(
        output.map { o =>
          frame.dropStack(methodRef.descriptor.args.size + 1).pushStack(o)
        } getOrElse {
          frame.dropStack(methodRef.descriptor.args.size + 1)
        },
        dataIn = frame.takeStack(methodRef.descriptor.args.size + 1).reverse.zip(inputs),
        dataValues = output.toSeq.flatMap { o => Seq(o -> Data(methodRef.descriptor.ret, None)) }
      )
    override def updateValues(f: DataLabel.In => Data) =
      output.map(_ -> Data(methodRef.descriptor.ret, None)).toMap
  }
}

case class FrameUpdate(
  newFrame: Frame,
  dataIn: Seq[(DataLabel.Out, DataLabel.In)] = Seq.empty,
  dataValues: Seq[(DataLabel.Out, Data)] = Seq.empty
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
  final class In (name: String) extends DataLabel(name)
  final class Out(name: String) extends DataLabel(name)

  def in(name: String) = new In(name)
  def out(name: String) = new Out(name)
}

