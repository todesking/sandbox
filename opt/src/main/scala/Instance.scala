package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod, Modifier }

import com.todesking.scalapp.syntax._

sealed abstract class Instance[A <: AnyRef] {
  /*
  type ActualType <: A

  val classRef: ClassRef

  def methods: Set[LocalMethodRef]

  def hasMethod(name: String, descriptor: String): Boolean =
    hasMethod(name, MethodDescriptor.parse(descriptor))

  def hasMethod(name: String, descriptor: MethodDescriptor): Boolean =
    hasMethod(LocalMethodRef(name, descriptor))

  def hasMethod(ref: LocalMethodRef): Boolean =
    methods.contains(ref)

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
  */
  // TODO: return Success/Abstract/Native/UnSupportedOp
  def methodBody(ref: MethodRef): Option[MethodBody]

  def methods: Set[MethodRef]

  def hasMethod(ref: MethodRef): Boolean =
    methods.contains(ref)
  def hasMethod(ref: String): Boolean =
    hasMethod(MethodRef.parse(ref))

}
object Instance {
  def of[A <: AnyRef](value: A): Original[A] = Original(value)

  case class Original[A <: AnyRef](value: A) extends Instance[A] {
    require(value != null)

    def duplicate[B >: A <: AnyRef: ClassTag](): Duplicate[B] = {
      val baseClass = implicitly[ClassTag[B]].runtimeClass.asInstanceOf[Class[B]]
      val baseRef = ClassRef.of(baseClass)
      Duplicate[B](this, baseClass, jMethods.flatMap { case (mref, jm) =>
        if(ClassRef.of(jm.getDeclaringClass) < baseRef)
          methodBody(mref).map { b =>
            import Bytecode._
            mref -> b.rewrite {
              // TODO: invokespecial, fields
              case iv @ invokevirtual(cref, mref) if cref < baseRef =>
                invokevirtual(ClassRef.ThisRef, mref)
            }
          }
        else
          None
      }.toMap)
    }

    override def methodBody(ref: MethodRef): Option[MethodBody] =
      jMethods.get(ref).flatMap(MethodBody.parse(jClass, _))

    override def methods = jMethods.keySet

    def javaMethod(ref: MethodRef): JMethod =
      jMethods(ref)

    private[this] lazy val jClass = value.getClass
    private[this] lazy val jMethods: Map[MethodRef, JMethod] = {
      // TODO: private method
      jClass.getMethods.map { m => MethodRef.from(m) -> m }.toMap
    }
  }

  case class Duplicate[A <: AnyRef](
    orig: Original[_ <: A],
    baseClass: Class[A],
    methodBodies: Map[MethodRef, MethodBody]
  ) extends Instance[A] {
    require(orig != null)

    override def methodBody(ref: MethodRef): Option[MethodBody] =
      methodBodies.get(ref) orElse orig.methodBody(ref)

    override def methods = orig.methods ++ methodBodies.keySet

    def methodModified(m: MethodRef): Boolean =
      methodBodies.contains(m)

    private[this] def methodDefinitionClass(ref: MethodRef): Option[ClassRef] = {
      if(methodModified(ref)) None
      else Some(ClassRef.of(orig.javaMethod(ref).getDeclaringClass))
    }

    def materialize(): Original[A] = {
      import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod, CtConstructor, ByteArrayClassPath }
      import javassist.bytecode.{ Bytecode => JABytecode, MethodInfo }

      def ctClass(tr: TypeRef): CtClass = {
        tr match {
          case TypeRef.Int => CtClass.intType
          case unk => throw new NotImplementedError(s"${unk}")
        }
      }

      validate()

      val classLoader = baseClass.getClassLoader
      val className = makeUniqueName(classLoader, baseClass)
      val baseRef = ClassRef.of(baseClass)
      val classRef = ClassRef(className, classLoader)

      val classPool = new ClassPool(null)
      Instance.findMaterializedClasses(baseClass.getClassLoader).foreach { case (name, bytes) =>
        classPool.appendClassPath(new ByteArrayClassPath(name, bytes))
      }
      classPool.appendClassPath(new ClassClassPath(baseClass))

      val ctBase = classPool.get(baseClass.getName)

      val klass = classPool.makeClass(className, ctBase)
      val constPool = klass.getClassFile.getConstPool
      val ctObject = classPool.get("java.lang.Object")
      methods.filter { m =>
        methodModified(m) || methodDefinitionClass(m).getOrElse(classRef) < baseRef
      } foreach { ref =>
        methodBody(ref).fold(throw new AssertionError) { body =>
          val out = new JABytecode(constPool, 0, 0)
          val jumps = mutable.HashMap.empty[Int, (Int, JumpTarget)] // jump operand address -> (insn addr -> target)
          val addrs = mutable.HashMap.empty[Bytecode.Label, Int]
          import Bytecode._
          body.rewrite {
            case invokevirtual(ClassRef.ThisRef, m) =>
              invokevirtual(classRef, m)
          }.bytecode foreach { bc =>
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
                // TODO: check resolved class
                out.addInvokevirtual(className.str, methodRef.name, methodRef.descriptor.str)
              case if_icmple(target) =>
                out.add(0xA4)
                jumps(out.getSize) = (out.getSize - 1) -> target
                out.add(0x00, 0x03)
              case getfield(classRef, fieldRef) =>
                println("addGetfield")
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
      val ctor = new CtConstructor(Array.empty, klass)
      ctor.setBody(";")
      klass.addConstructor(ctor)

      // TODO: Move to JavasssitUtil
      // TODO: Make getItem to public
      def printcp(cfile: javassist.bytecode.ClassFile): Unit = {
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

      val value = klass.toClass(classLoader, null).newInstance().asInstanceOf[A]
      val bytes = klass.toBytecode
      Instance.registerMaterialized(classLoader, klass.getName, bytes)
      Instance.of(value)
    }

    private[this] def makeUniqueName(cl: ClassLoader, klass: Class[_]): String = {
      // TODO: :(
      klass.getName + "_" + System.identityHashCode(this)
    }

    private[this] def validate(): Unit = {
      def fail(msg: String) =
        throw new IllegalStateException(msg)

      if((baseClass.getModifiers & Modifier.FINAL) == Modifier.FINAL)
        fail("base is final class")
      // TODO: check finalizer
      // * for each fields `f` in `x`:
      //   * FAIL if `f` is non-final and `x` is _escaped_
      //   * if `f` defined at `_ <<: X`
      //     * FAIL if
      //       * `f` has type `_ <<: X`
      // * for each ALL methods/constructors `m` in `x`:
      //   * FAIL if
      //     * `m` is abstract
      //     * `m` takes parameter `_ <<: X`
      //     * `m` returns `_ <<: X`
      //     * `m` has non-this reference `_ <<: X`
      // * for each visible or self-referenced non-constructor methods `m` in `x`:
      //   * if `m` defined at `_ <<: X`
      //     * FAIL if
      //       * `m` is native
      //       * `m` leaks `this` as `_ <<: X`
      // * for each constructor/used super constructor `c` in `x`:
      //   * FAIL if ANY OF
      //     * `c` is native
      //     * `c` may have side-effect
    }
  }

  // TODO: Weaken CL
  private[this] val materializedClasses = mutable.HashMap.empty[(ClassLoader, String), Array[Byte]]
  def registerMaterialized(cl: ClassLoader, name: String, bytes: Array[Byte]): Unit = synchronized {
    if(materializedClasses.contains(cl -> name))
      throw new IllegalArgumentException(s"${name} is already defined in ${cl}")
    materializedClasses(cl -> name) = bytes
  }
  // TODO: Resolve name conflict
  def findMaterializedClasses(cl: ClassLoader): Seq[(String, Array[Byte])] = synchronized {
    if(cl == null) {
      Seq.empty
    } else {
      materializedClasses.collect { case ((l, n), b) if l == cl => (n -> b) }.toSeq ++
        findMaterializedClasses(cl.getParent)
    }
  }

  /*
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
  */
}

