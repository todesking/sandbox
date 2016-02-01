package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{Method => JMethod}

import com.todesking.scalapp.syntax._

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
          val out = new Bytecode(constPool, 0, 0)
          val bc = body.toBytecode(new LocalAllocator(body))
          bc.pp()
          val jumps = mutable.HashMap.empty[Int, (Int, JumpTarget)] // jump operand address -> (insn addr -> target)
          val jumpAddresses = mutable.HashMap.empty[JumpTarget, Int] // target -> target address
          import AbstractBytecode._
          bc.instructions foreach {
            case ireturn() =>
              out.addReturn(CtClass.intType)
            case iload(n) =>
              out.addIload(n)
            case aload(n) =>
              out.addAload(n)
            case istore(n) =>
              out.addIstore(n)
            case iconst(c) =>
              out.addIconst(c)
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
          out.setMaxLocals(bc.maxLocals)
          out.setMaxStack(bc.maxStackDepth)
          val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
          minfo.setCodeAttribute(out.toCodeAttribute)
          klass.getClassFile.addMethod(minfo)
        }
      }
      // TODO: fields

      klass.toClass(new java.net.URLClassLoader(Array.empty, jClass.getClassLoader), null).newInstance().asInstanceOf[A]
    }
  }
}

