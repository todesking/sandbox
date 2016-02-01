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
    dataFlow.pp()
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
    new MethodBodyBytecode(ainsns, la.size, 100)
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
        if(isStatic) Frame(argLabels, List.empty, Effect.fresh())
        else Frame(DataLabel.out("this") +: argLabels, List.empty, Effect.fresh())
      val insns = mutable.ArrayBuffer.empty[Instruction]
      val index2ILabel = mutable.HashMap.empty[Int, InstructionLabel]
      val dataFlow = mutable.ArrayBuffer.empty[(DataLabel.Out, DataLabel.In)]

      val ops = mutable.HashMap.empty[JumpTarget, Frame => FrameUpdate]
      val controls = new mutable.HashMap[JumpTarget, mutable.Set[JumpTarget]] with mutable.MultiMap[JumpTarget, JumpTarget]
      var prevTarget: JumpTarget = null

      val addr2jumpTargets = mutable.HashMap.empty[Int, JumpTarget]
      def jumpTargetFromAddr(addr: Int): JumpTarget =
        addr2jumpTargets.get(addr).getOrElse {
          val j = JumpTarget.fresh()
          addr2jumpTargets(addr) = j
          j
        }

      def onExchange(addr: Int, name: String)(f: Frame => Frame): Unit = {
        println(s"exch ${addr}: ${name}")
        val jt = jumpTargetFromAddr(addr)
        ops(jt) = {frame: Frame => println(s"frame for ${name}"); FrameUpdate(f(frame)) }
        if(prevTarget != null) controls.addBinding(prevTarget, jt)
        prevTarget = jt
      }

      def onInstruction(addr: Int, i: Instruction): Unit = {
        println(s"insn ${addr}: ${i}")
        insns += i
        val jt = jumpTargetFromAddr(addr)
        ops(jt) = { f: Frame => println(s"frame for ${i}"); i.nextFrame(f) }
        if(prevTarget != null) controls.addBinding(prevTarget, jt)
        prevTarget = jt
        i match {
          case Instruction.IfICmpLE(th, el) =>
            controls.addBinding(jt, th)
            controls.addBinding(jt, el)
            prevTarget = null
          case _ =>
        }
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
            onExchange(index, "nop")(identity)
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
          case 0x10 => // bipush
            onInstruction(index, Instruction.Const(TypeRef.Int, it.byteAt(index + 1)))
          // TODO
          case 0x1B => // iload_1
            onExchange(index, "iload_1") { frame =>
              frame.pushStack(frame.locals(1))
            }
          // TODO
          case 0xA4 => // if_icmple
            onInstruction(
              index,
              Instruction.IfICmpLE(
                jumpTargetFromAddr(index + it.s16bitAt(index + 1)),
                jumpTargetFromAddr(it.lookAhead())
              )
            )
          // TODO
          case 0x2A => // aload_0
            onExchange(index, "aload_0") { frame =>
              frame.pushStack(frame.locals(0))
            }
          // TODO
          case 0xA7 => // goto
            onInstruction(index, Instruction.Goto(jumpTargetFromAddr(it.u16bitAt(index + 1))))
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

      val done = mutable.Set.empty[(JumpTarget, Frame)]
      val tasks = mutable.Set.empty[(JumpTarget, Frame)]
      val beforeFrames = mutable.HashMap.empty[JumpTarget, Frame]
      tasks.add(jumpTargetFromAddr(0) -> initialFrame)
      while(tasks.nonEmpty) {
        val (target, frame) = tasks.head
        tasks.remove(target -> frame)
        done += (target -> frame)
        val update = ops(target).apply(frame)
        beforeFrames(target) = frame
        update.dataIn foreach { dataFlow += _ }
        controls.get(target).getOrElse(Set.empty) foreach { t =>
          if(!done.contains(t -> update.newFrame)) {
            tasks.add(t -> update.newFrame)
          }
        }
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

