package com.todesking.hoge

import javassist.{ClassPool, CtClass}
import javassist.bytecode.{CodeAttribute, ConstPool, Bytecode => JABytecode, MethodInfo}
import java.lang.reflect.{Method => JMethod}

import scala.collection.mutable

object Javassist {
  def compile(classPool: ClassPool, constPool: ConstPool, body: MethodBody): CodeAttribute = {
    val ctObject = classPool.get("java.lang.Object")
    val out = new JABytecode(constPool, 0, 0)
    val jumps = mutable.HashMap.empty[Int, (Int, JumpTarget)] // jump operand address -> (insn addr -> target)
    val addrs = mutable.HashMap.empty[Bytecode.Label, Int]
    import Bytecode._
    def concrete(r: ClassRef): ClassRef.Concrete = r match {
      case c: ClassRef.Concrete => c
      case unk => throw new AssertionError(s"Not concrete class reference: ${unk}")
    }
    body.bytecode foreach { bc =>
      addrs(bc.label) = out.getSize
      bc match {
        case nop() =>
          out.add(0x00)
        case aconst_null() =>
          out.addConstZero(ctObject)
        case vreturn() =>
          out.addReturn(null)
        case ireturn() =>
          out.addReturn(CtClass.intType)
        case lreturn() =>
          out.addReturn(CtClass.longType)
        case areturn() =>
          out.add(0xB0)
        case iload(n) =>
          out.addIload(n)
        case aload(n) =>
          out.addAload(n)
        case istore(n) =>
          out.addIstore(n)
        case astore(n) =>
          out.addAstore(n)
        case iconst(c) =>
          out.addIconst(c)
        case lconst(c) =>
          out.addLconst(c)
        case goto(target) =>
          out.add(0xA7)
          jumps(out.getSize) = (out.getSize - 1) -> target
          out.add(0x00, 0x03)
        case dup() =>
          out.add(0x59)
        case pop() =>
          out.add(0x57)
        case iadd() =>
          out.add(0x60)
        case invokevirtual(classRef, methodRef) =>
          // TODO: check resolved class
          out.addInvokevirtual(concrete(classRef).str, methodRef.name, methodRef.descriptor.str)
        case if_icmple(target) =>
          out.add(0xA4)
          jumps(out.getSize) = (out.getSize - 1) -> target
          out.add(0x00, 0x03)
        case getfield(classRef, fieldRef) =>
          println("addGetfield")
          out.addGetfield(concrete(classRef).str, fieldRef.name, fieldRef.descriptor.str)
      }
    }
    jumps foreach {
      case (dataIndex, (index, target)) =>
        val label = body.jumpTargets(target)
        val targetIndex = addrs(label)
        out.write16bit(dataIndex, targetIndex - index)
    }
    out.setMaxLocals(body.maxLocals)
    out.setMaxStack(body.maxStackDepth)
    out.toCodeAttribute
  }

  def decompile(m: JMethod): Option[MethodBody] = {
    require(m != null)

    import javassist.{ ClassPool, ClassClassPath, ByteArrayClassPath, CtClass, CtMethod }

    val jClass = m.getDeclaringClass
    val classPool = new ClassPool(null)
    Instance.findMaterializedClasses(jClass.getClassLoader).foreach { case (name, bytes) =>
      classPool.appendClassPath(new ByteArrayClassPath(name, bytes))
    }
    classPool.appendClassPath(new ClassClassPath(jClass))

    val ctClass = classPool.get(jClass.getName)
    val mRef = MethodRef.from(m)

    val ctMethod = ctClass.getMethod(mRef.name, mRef.descriptor.str)

    if (ctMethod.isEmpty) { // "abstract or native"(from CtClass doc)
      None
    } else if (ctMethod.getMethodInfo2.getCodeAttribute == null) { // ??? but it happens
      None
    } else {
      val isStatic = (ctMethod.getMethodInfo2.getAccessFlags & 0x08) == 0x08

      val codeAttribute = ctMethod.getMethodInfo2.getCodeAttribute
      val it = codeAttribute.iterator
      val cpool = ctMethod.getDeclaringClass.getClassFile.getConstPool
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
            onInstruction(index, iconst(it.signedByteAt(index + 1)))
          // TODO
          case 0x1B => // iload_1
            onInstruction(index, iload(1))
          // TODO
          case 0x2B => // aload_1
            onInstruction(index, aload(1))
          // TODO
          case 0x2A => // aload_0
            onInstruction(index, aload(0))
          // TODO
          case 0x3C => // istore_1
            onInstruction(index, istore(1))
          // TODO
          case 0xA4 => // if_icmple
            onInstruction(
              index,
              if_icmple(addr2jt(index + it.s16bitAt(index + 1)))
            )
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
          case 0x60 =>
            onInstruction(index, iadd())
          // TODO
          case 0xB4 => // getfield
            val constIndex = it.u16bitAt(index + 1)
            println(s"getField " + constIndex)
            val className = cpool.getFieldrefClassName(constIndex)
            val classRef = ClassRef.of(jClass.getClassLoader.loadClass(className))
            val fieldName = cpool.getFieldrefName(constIndex)
            val fieldDescriptor = FieldDescriptor.parse(cpool.getFieldrefType(constIndex), jClass.getClassLoader)
            val fieldRef = FieldRef(fieldName, fieldDescriptor)
            onInstruction(index, getfield(classRef, fieldRef))
          // TODO
          case 0xB1 => // return
            onInstruction(index, vreturn())
          // TODO
          case 0xB0 => // areturn
            onInstruction(index, areturn())
          // TODO
          case 0xB6 => // invokevirtual
            val constIndex = it.u16bitAt(index + 1)
            val className = cpool.getMethodrefClassName(constIndex)
            val methodName = cpool.getMethodrefName(constIndex)
            val methodType = cpool.getMethodrefType(constIndex)
            val classRef = ClassRef.of(jClass.getClassLoader.loadClass(className))
            onInstruction(
              index,
              invokevirtual(
                classRef,
                MethodRef(methodName, MethodDescriptor.parse(methodType, jClass.getClassLoader)))
            )
          case unk =>
            throw new UnsupportedOpcodeException(unk, m.getName)
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

  // TODO: Make javassist getItem to public
  def printConstPool(cfile: javassist.bytecode.ClassFile): Unit = {
    val cop = cfile.getConstPool
    val gi = cop.getClass.getDeclaredMethods.find(_.getName == "getItem").get
    gi.setAccessible(true)
    (1 until cop.getSize) foreach { i =>
      val a = gi.invoke(cop, i.asInstanceOf[java.lang.Integer])
      val x = a.getClass.getMethods.find(_.getName == "print").get
      x.setAccessible(true)
      val pw = new java.io.PrintWriter(System.out)
      println(s"${i} -> ${a.getClass}")
      print("  ")
      x.invoke(a, pw)
      pw.flush()
    }
  }
}
