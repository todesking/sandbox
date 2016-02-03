package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{Method => JMethod}

import com.todesking.scalapp.syntax._

sealed abstract class Instance[A <: AnyRef] {
  def methods: Set[LocalMethodRef]

  def hasMethod(name: String, descriptor: String): Boolean =
    hasMethod(name, MethodDescriptor.parse(descriptor))

  def hasMethod(name: String, descriptor: MethodDescriptor): Boolean =
    hasMethod(LocalMethodRef(name, descriptor))

  def hasMethod(ref: LocalMethodRef): Boolean =
    methods.contains(ref)

  def methodBody(ref: LocalMethodRef): Option[MethodBody]

  def methodModified(ref: LocalMethodRef): Boolean

  def baseClass: Class[_ <: A]

  def instance(): A
}
object Instance {
  case class Native[A <: AnyRef](value: A) extends Instance[A] {
    require(value != null)

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

    override def baseClass = value.getClass

    override def instance(): A =
      value

    override def methodModified(ref: LocalMethodRef) = false

  }

  case class Rewritten[A <: AnyRef](
    base: Instance[A],
    methodBodies: Map[LocalMethodRef, MethodBody] = Map.empty
  ) extends Instance[A] {
    override def methods = base.methods
    override def methodBody(ref: LocalMethodRef) =
      methodBodies.get(ref) orElse base.methodBody(ref)
    override def methodModified(ref: LocalMethodRef) =
      methodBodies.get(ref).map(_ => true) getOrElse base.methodModified(ref)
    override def baseClass = base.baseClass
    override def instance() = {
      import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod}
      import javassist.bytecode.{Bytecode => JABytecode, MethodInfo}

      val classPool = new ClassPool(null)
      classPool.appendClassPath(new ClassClassPath(baseClass))

      val ctBase = classPool.get(baseClass.getName)
      // TODO make name unique
      val klass = classPool.makeClass(baseClass.getName + "_rewritten", ctBase)
      val constPool = klass.getClassFile.getConstPool

      def ctClass(tr: TypeRef): CtClass = {
        tr match {
          case TypeRef.Int => CtClass.intType
          case unk => throw new NotImplementedError(s"${unk}")
        }
      }

      val ctObject = classPool.get("java.lang.Object")

      methods.filter(methodModified).map(methodBody)

      methods.filter(methodModified) foreach { ref =>
        methodBody(ref) foreach { body =>
          val out = new JABytecode(constPool, 0, 0)
          val jumps = mutable.HashMap.empty[Int, (Int, JumpTarget)] // jump operand address -> (insn addr -> target)
          val addrs = mutable.HashMap.empty[Bytecode.Label, Int]
          import Bytecode._
          body.bytecode foreach { bc =>
            (out.getSize -> bc).pp()
            addrs(bc.label) = out.getSize
            bc match {
              case nop() =>
                out.add(0x00)
              case aconst_null() =>
                out.addConstZero(ctObject)
              case ireturn() =>
                out.addReturn(CtClass.intType)
              case lreturn() =>
                out.addReturn(CtClass.longType)
              case iload(n) =>
                out.addIload(n)
              case aload(n) =>
                out.addAload(n)
              case istore(n) =>
                out.addIstore(n)
              case iconst(c) =>
                out.addIconst(c)
              case lconst(c) =>
                out.addLconst(c)
              case goto(target) =>
                out.add(0xA7)
                jumps(out.getSize) = (out.getSize - 1) -> target
                out.add(0x00, 0x03)
              case invokevirtual(className, methodRef) =>
                out.addInvokevirtual(className.str, methodRef.name, methodRef.descriptor.str)
              case if_icmple(target) =>
                out.add(0xA4)
                jumps(out.getSize) = (out.getSize - 1) -> target
                out.add(0x00, 0x03)
            }
          }
          body.pp()
          jumps.pp()
          addrs.pp()
          jumps foreach { case (dataIndex, (index, target)) =>
            val label = body.jumpTargets(target)
            val targetIndex = addrs(label)
            out.write16bit(dataIndex, targetIndex - index)
          }
          out.setMaxLocals(body.maxLocals)
          out.setMaxStack(body.maxStackDepth)
          val ca = out.toCodeAttribute
          val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
          minfo.setCodeAttribute(ca)
          val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, minfo)
          ca.setAttribute(sm)
          klass.getClassFile.addMethod(minfo)
        }
      }
      // TODO: fields

      klass.toClass(new java.net.URLClassLoader(Array.empty, baseClass.getClassLoader), null).newInstance().asInstanceOf[A]
    }
  }
}

