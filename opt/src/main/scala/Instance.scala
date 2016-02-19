package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod, Modifier }

import com.todesking.scalapp.syntax._

sealed abstract class Instance[+A <: AnyRef] {
  type ActualType <: A

  val classRef: ClassRef

  def methods: Set[LocalMethodRef]

  def hasMethod(name: String, descriptor: String): Boolean =
    hasMethod(name, MethodDescriptor.parse(descriptor))

  def hasMethod(name: String, descriptor: MethodDescriptor): Boolean =
    hasMethod(LocalMethodRef(name, descriptor))

  def hasMethod(ref: LocalMethodRef): Boolean =
    methods.contains(ref)

  def methodBody(ref: LocalMethodRef): Option[MethodBody]

  def methodModified(ref: LocalMethodRef): Boolean

  def baseClass: Class[ActualType]

  def instance(): A

  // false if unsure
  def sameMethodDefined[B <: AnyRef](method: LocalMethodRef, other: Instance[B]): Boolean =
    this.hasMethod(method) && other.hasMethod(method) &&
      !this.methodModified(method) && !other.methodModified(method) &&
      method.getJavaMethod(this.baseClass) == method.getJavaMethod(other.baseClass)

  def isNativeMethod(methodRef: LocalMethodRef): Boolean =
    methodModifiers(methodRef).map { mod => (mod & Modifier.NATIVE) == Modifier.NATIVE }.getOrElse(???)

  def isAbstractMethod(methodRef: LocalMethodRef): Boolean =
    methodModifiers(methodRef).map { mod => (mod & Modifier.ABSTRACT) == Modifier.ABSTRACT }.getOrElse(???)

  def methodModifiers(m: LocalMethodRef): Option[Int] =
    if(methodModified(m)) None // TODO
    else m.getJavaMethod(baseClass).map(_.getModifiers)
}
object Instance {
  case class Native[+A <: AnyRef](value: A) extends Instance[A] {
    require(value != null)

    private[this] val javaClass = value.getClass

    private[this] def javaMethod(ref: LocalMethodRef): Option[JMethod] =
      javaClass.getMethods.find { m => LocalMethodRef.from(m) == ref }

    private[this] val methodBodies = mutable.HashMap.empty[LocalMethodRef, Option[MethodBody]]

    override val classRef = ClassRef.of(javaClass)

    override def baseClass = value.getClass.asInstanceOf[Class[ActualType]]

    override val methods =
      javaClass.getMethods.map(LocalMethodRef.from).toSet

    override def methodBody(ref: LocalMethodRef): Option[MethodBody] = synchronized {
      methodBodies.get(ref) getOrElse {
        val body = javaMethod(ref).flatMap(MethodBody.parse(value.getClass, _))
        methodBodies(ref) = body
        body
      }
    }

    override def instance(): A =
      value

    override def methodModified(ref: LocalMethodRef) = false
  }

  case class New[A <: AnyRef](override val baseClass: Class[A], override val classRef: ClassRef) extends Instance[A] {
    try  {
      baseClass.getConstructor()
    } catch {
      case e: NoSuchMethodException =>
        throw new IllegalArgumentException(s"class ${baseClass} have not 0-ary constructor")
    }

    override type ActualType = A

    override val methods: Set[LocalMethodRef] =
      baseClass.getMethods.map(LocalMethodRef.from).toSet

    override def methodBody(ref: LocalMethodRef): Option[MethodBody] =
      ref.getJavaMethod(baseClass).flatMap(MethodBody.parse(baseClass, _))

    override def methodModified(ref: LocalMethodRef): Boolean = false

    override def instance(): A =
      baseClass.newInstance()
  }

  case class Rewritten[+A <: AnyRef](
      base: Instance[A],
      methodBodies: Map[LocalMethodRef, MethodBody] = Map.empty,
      useBaseClassRef: Boolean = false
  ) extends Instance[A] {
    override type ActualType = base.ActualType
    override val classRef = if(useBaseClassRef) base.classRef else ClassRef.newAnonymous(base.classRef.classLoader, baseClass.getName)
    override def methods = base.methods ++ methodBodies.keySet
    override def methodBody(ref: LocalMethodRef) =
      methodBodies.get(ref) orElse base.methodBody(ref)
    override def methodModified(ref: LocalMethodRef) =
      methodBodies.contains(ref) || base.methodModified(ref)
    override def baseClass = base.baseClass
    override lazy val instance = {
      import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod, CtConstructor }
      import javassist.bytecode.{ Bytecode => JABytecode, MethodInfo }

      // TODO check baseClass has 0-ary constructor

      val classPool = new ClassPool(null)
      classPool.appendClassPath(new ClassClassPath(baseClass))

      val ctBase = classPool.get(baseClass.getName)
      // TODO make name unique
      val klass = classPool.makeClass(classRef.name, ctBase)
      val constPool = klass.getClassFile.getConstPool

      def ctClass(tr: TypeRef): CtClass = {
        tr match {
          case TypeRef.Int => CtClass.intType
          case unk => throw new NotImplementedError(s"${unk}")
        }
      }

      val ctObject = classPool.get("java.lang.Object")

      methods.filter(methodModified) foreach { ref =>
        methodBody(ref).fold(throw new AssertionError) { body =>
          val out = new JABytecode(constPool, 0, 0)
          val jumps = mutable.HashMap.empty[Int, (Int, JumpTarget)] // jump operand address -> (insn addr -> target)
          val addrs = mutable.HashMap.empty[Bytecode.Label, Int]
          import Bytecode._
          body.bytecode foreach { bc =>
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
              case invokevirtual(className, methodRef) =>
                out.addInvokevirtual(className.str, methodRef.name, methodRef.descriptor.str)
              case if_icmple(target) =>
                out.add(0xA4)
                jumps(out.getSize) = (out.getSize - 1) -> target
                out.add(0x00, 0x03)
              case getfield(classRef, fieldRef) =>
                out.addGetfield(classRef.str, fieldRef.name, fieldRef.descriptor.str)
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
          val ca = out.toCodeAttribute
          val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
          minfo.setCodeAttribute(ca)
          val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, minfo)
          ca.setAttribute(sm)
          klass.getClassFile.addMethod(minfo)
        }
      }
      // TODO: fields

      val ctor = new CtConstructor(Array.empty, klass)
      ctor.setBody("super();")
      klass.addConstructor(ctor)

      klass.toClass(classRef.classLoader, null).newInstance().asInstanceOf[A]
    }
  }
}

