package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{Method => JMethod}

import com.todesking.scalapp.syntax._

case class MethodBody(
  isStatic: Boolean,
  descriptor: MethodDescriptor,
  bytecode: Seq[Bytecode],
  jumpTargets: Map[JumpTarget, Bytecode.Label],
  maxLocals: Int,
  maxStackDepth: Int
)

/*
sealed abstract class Operation {
  final val label =Operation.Label.fresh()
  val inputs: Seq[DataLabel.In]
  val output: Option[DataLabel.Out]
}
object Operation {
  class Label private() extends AbstractLabel
  object Label {
    def fresh(): Label = new Label
  }
}
case class Dataflow(
  descriptor: MethodDescriptor,
  thisData: Option[DataLabel.In],
  args: Seq[DataLabel.In],
  ret: Option[DataLabel.Out],
  connection: Map[DataLabel.In, DataLabel.Out],
  operations: Seq[Operation]
) {
  require(args.size == descriptor.args.size)
  require(!(ret.isEmpty ^ descriptor.isVoid))

  private[this] lazy val dataValues: Map[DataLabel, Data] = {
    val m = mutable.HashMap.empty[DataLabel, Data]

    thisData foreach { t => m(t) = Data(TypeRef.This, None) }
    args.zip(descriptor.args) foreach { case (l, t) => m(l) = Data(t, None) }

    val in2out = connection.map { case (a, b) => (b -> a) }.toMap
    Util.tsort(operations)(_.label) { o => o.inputs.map(connection).flatMap {o => operations.find(_.output == Some(o)) } } foreach { o =>
      o.inputs foreach { in => m(in) = m(in2out(in)) }
      o.updateValues(m) foreach { case (k, v) => m(k) = v }
    }
    m.toMap
  }

  def dataAliases(l: DataLabel): Set[DataLabel] =
    l match {
      case l: DataLabel.In => dataFlow.filter(_._2 == l).map(_._1).toSet + l
      case l: DataLabel.Out => dataFlow.filter(_._1 == l).map(_._2).toSet + l
    }

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

  def toBytecode(la: LocalAllocator): MethodBodyBytecode = {
    import AbstractBytecode._
    val ainsns =
      InstructionSerializer.serialize(this).flatMap {
        case i @ Instruction.Return() =>
          if(data(i.retVal).typeRef == TypeRef.Int)
            Seq(
              iload(la(i.retVal)),
              ireturn()
            )
          else
            ???
        case i @ Instruction.Const(tref, value) =>
          if(tref == TypeRef.Int) {
            Seq(
              iconst(value.asInstanceOf[Int]),
              istore(la(i.output))
            )
          } else ???
        case i @ Instruction.Goto(target) =>
          Seq(goto(target))
        case i @ Instruction.IfICmpLE(th, el) =>
          Seq(
            iload(la(i.value1)),
            iload(la(i.value2)),
            if_icmple(th),
            goto(el)
          )
        case i @ Instruction.InvokeVirtual(className, methodRef) =>
          i.argAndType.map { case (label, tpe) =>
            load(tpe, la(label))
          } ++ Seq(
            invokevirtual(className, methodRef)
          ) ++ i.output.map { ret => store(methodRef.ret, la(ret)) }.toSeq
        case unk =>
          throw new NotImplementedError(unk.toString)
      }
    new MethodBodyBytecode(ainsns, Map.empty, la.size, 100)
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
      s"""${dataNames.id(i)}[label="${i.name}: ${try { data(i) } catch { case e: IllegalArgumentException => "BUG: Not found" }}"];"""
    }
  }.mkString("\n")
}
${
  initialFrame.locals.distinct.map { o =>
    s"""${dataNames.id(o)}[label="${o.name}: ${try { data(o) } catch { case e : IllegalArgumentException=> "BUG: Not found" }}"];"""
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
*/

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

      val codeAttribute = ctMethod.getMethodInfo2.getCodeAttribute
      val it = codeAttribute.iterator
      val cpool = ctClass.getClassFile2.getConstPool
      val bcs = mutable.ArrayBuffer.empty[Bytecode]
      val jumpTargets = mutable.HashMap.empty[JumpTarget, Bytecode.Label]
      val addr2jt = new AbstractLabel.Assigner[Int, JumpTarget](JumpTarget.fresh())

      def onInstruction(index: Int, bc: Bytecode): Unit = {
        bcs += bc
        jumpTargets(addr2jt(index)) = bc.label
      }

      while(it.hasNext) {
        val index = it.next()
        import Bytecode._
        it.byteAt(index) match {
          case 0x00 => // nop
            onInstruction(index, nop())
          case 0x01 => // aconst_null
            onInstruction(index, aconst_null())
          // TODO
          case 0x03 => // iconst_0
            onInstruction(index, iconst(0))
          case 0x04 => // iconst_1
            onInstruction(index, iconst(1))
          case 0x05 => // iconst_2
            onInstruction(index, iconst(2))
          case 0x06 => // iconst_3
            onInstruction(index, iconst(3))
          case 0x07 => // iconst_4
            onInstruction(index, iconst(4))
          case 0x08 => // iconst_5
            onInstruction(index, iconst(5))
          case 0x09 => // lconst_0
            onInstruction(index, lconst(0))
          // TODO
          case 0x10 => // bipush
            // TODO: signed?
            onInstruction(index, iconst(it.signedByteAt(index + 1)))
          // TODO
          case 0x1B => // iload_1
            onInstruction(index, iload(1))
          // TODO
          case 0xA4 => // if_icmple
            onInstruction(
              index,
              if_icmple(addr2jt(index + it.s16bitAt(index + 1)))
            )
          // TODO
          case 0x2A => // aload_0
            onInstruction(index, aload(0))
          // TODO
          case 0xA7 => // goto
            onInstruction(index, goto(addr2jt(index + it.s16bitAt(index + 1))))
          // TODO
          case 0xAC => // ireturn
            onInstruction(index, ireturn())
          // TODO
          case 0xAD => // lreturn
            onInstruction(index, lreturn())
          // TODO
          case 0xB6 => // invokevirtual
            val constIndex = it.u16bitAt(index+1)
            val className = cpool.getMethodrefClassName(constIndex)
            val methodName = cpool.getMethodrefName(constIndex)
            val methodType = cpool.getMethodrefType(constIndex)
            onInstruction(
              index,
              invokevirtual(ClassName(className), LocalMethodRef(methodName, MethodDescriptor.parse(methodType)))
            )
          case unk =>
            throw new UnsupportedOpcodeException(unk)
        }
      }
      Some(MethodBody(
        isStatic,
        mRef.descriptor,
        bcs.toSeq,
        jumpTargets.toMap,
        codeAttribute.getMaxLocals,
        codeAttribute.getMaxStack
      ))
    }
  }
}

