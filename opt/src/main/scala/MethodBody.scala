package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._

case class MethodBody(
  isStatic: Boolean,
  descriptor: MethodDescriptor,
  bytecode: Seq[Bytecode],
  jumpTargets: Map[JumpTarget, Bytecode.Label],
  maxLocals: Int,
  maxStackDepth: Int
) {
  require(bytecode.nonEmpty)

  lazy val dataflow: Dataflow =
    Dataflow.build(this)

  lazy val initialFrame: Frame = {
    val initialEffect = Effect.fresh()
    val thisData = if(isStatic) None else Some(DataLabel.out("this") -> Data(TypeRef.This, None))
    val argData = descriptor.args.zipWithIndex.flatMap {
      case (t, i) =>
        val data = Data(t, None)
        val label = DataLabel.out(s"arg_${i}")
        if(t.isDoubleWord)
          Seq((DataLabel.out(s"second word of ${label.name}") -> data.secondWordData), (label -> data))
        else
          Seq(label -> data)
    }
    Frame((thisData.toSeq ++ argData).zipWithIndex.map(_.swap).toMap, List.empty, initialEffect)
  }

  def dataValue(l: DataLabel): Data =
    dataValues(l)

  // Yes I know this is just a pattern matching, not type-annotation, but I need readability
  lazy val (
    dataBinding: Map[DataLabel.In, DataLabel.Out],
    dataValues: Map[DataLabel, Data],
    dataMerges: Map[(Bytecode.Label, DataLabel.Out), Set[DataLabel.Out]],
    effectDependencies: Map[Bytecode.Label, Effect],
    effectMerges: Map[(Bytecode.Label, Effect), Set[Effect]],
    liveBytecode: Seq[Bytecode]
  ) = {
    val dataMerges = new AbstractLabel.Merger[Bytecode.Label, DataLabel.Out](DataLabel.out("merged"))
    val effectMerges = new AbstractLabel.Merger[Bytecode.Label, Effect](Effect.fresh())
    def mergeData(pos: Bytecode.Label, d1: (DataLabel.Out, Data), d2: (DataLabel.Out, Data)): (DataLabel.Out, Data) =
      (dataMerges.merge(pos, d1._1, d2._1) -> Data.merge(d1._2, d2._2))
    def merge(pos: Bytecode.Label, f1: Frame, f2: Frame): Frame = {
      Frame(
        (f1.locals.keySet ++ f2.locals.keySet)
          .filter { k => f1.locals.contains(k) && f2.locals.contains(k) }
          .map { k => (k -> mergeData(pos, f1.locals(k), f2.locals(k))) }.toMap,
        f1.stack.zip(f2.stack).map { case(a, b) => mergeData(pos, a, b) },
        effectMerges.merge(pos, f1.effect, f2.effect)
      )
    }

    val preFrames = mutable.HashMap.empty[Bytecode.Label, Frame]
    val effectDependencies = mutable.HashMap.empty[Bytecode.Label, Effect]
    val binding = mutable.HashMap.empty[DataLabel.In, DataLabel.Out]
    val dataValues = mutable.HashMap.empty[DataLabel, Data]
    val liveBcs = mutable.HashMap.empty[Bytecode.Label, Bytecode]

    val tasks = mutable.Set.empty[(Bytecode.Label, Frame)]
    tasks += (bytecode.head.label -> initialFrame)

    while(tasks.nonEmpty) {
      val (pos, frame) = tasks.head
      tasks.remove(pos -> frame)
      val merged = preFrames.get(pos).map(merge(pos, _, frame)) getOrElse frame
      if(preFrames.get(pos).map(_ != merged) getOrElse true) {
        preFrames(pos) = merged
        val bseq = bytecode.dropWhile(_.label != pos)
        val bc = bseq.head
        liveBcs(bc.label) = bc
        val u = bc.nextFrame(merged)
        binding ++= u.binding
        effectDependencies ++= u.effectDependencies
        dataValues ++= u.dataValues
        bseq.head match {
          case j: Bytecode.Jump =>
            tasks += (jumpTargets(j.target) -> u.newFrame)
          case b:  Bytecode.Branch =>
            tasks += (jumpTargets(b.target) -> u.newFrame)
            tasks += (bseq(1).label -> u.newFrame)
          case r: Bytecode.XReturn =>
          case _: Bytecode.Procedure | _: Bytecode.Shuffle =>
            tasks += (bseq(1).label -> u.newFrame)
        }
      }
    }

    dataMerges.toMap foreach { case ((pos, merged), ds) =>
      dataValues(merged) = ds.map(dataValues).reduceLeft[Data] { case (a, b) => Data.merge(a, b) }
    }

    (binding.toMap, dataValues.toMap, dataMerges.toMap, effectDependencies.toMap, effectMerges.toMap, liveBcs.values.toSeq)
  }
}

object MethodBody {
  def parse(jClass: Class[_], m: JMethod): Option[MethodBody] = {
    require(m != null)

    import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod }
    val classPool = new ClassPool(null)
    classPool.appendClassPath(new ClassClassPath(jClass))

    val ctClass = classPool.get(jClass.getName)
    val mRef = LocalMethodRef.from(m)

    val ctMethod = ctClass.getMethod(mRef.name, mRef.descriptor.str)

    if (ctMethod.isEmpty) { // "abstract or native"(from CtClass doc)
      None
    } else if (ctMethod.getMethodInfo2.getCodeAttribute == null) { // ??? but it happens
      None
    } else {
      val isStatic = (ctMethod.getMethodInfo2.getAccessFlags & 0x08) == 0x08

      val codeAttribute = ctMethod.getMethodInfo2.getCodeAttribute
      val it = codeAttribute.iterator
      val cpool = ctClass.getClassFile2.getConstPool
      val bcs = mutable.ArrayBuffer.empty[Bytecode]
      val jumpTargets = mutable.HashMap.empty[JumpTarget, Bytecode.Label]
      val addr2jt = JumpTarget.assigner[Int]()

      def onInstruction(index: Int, bc: Bytecode): Unit = {
        bcs += bc
        jumpTargets(addr2jt(index)) = bc.label
      }

      while (it.hasNext) {
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
            val constIndex = it.u16bitAt(index + 1)
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

