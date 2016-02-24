package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod, Modifier, Constructor }

import com.todesking.scalapp.syntax._

sealed abstract class Instance[A <: AnyRef] {
  // TODO: return Success/Abstract/Native/UnSupportedOp
  def methodBody(ref: MethodRef): Option[MethodBody]
  def methodBody(classRef: ClassRef, methodRef: MethodRef): Option[MethodBody]

  def classLoader: ClassLoader

  def virtualMethods: Map[MethodRef, MethodAttribute]
  def allMethods: Map[(ClassRef, MethodRef), MethodAttribute]

  def hasVirtualMethod(ref: MethodRef): Boolean =
    virtualMethods.contains(ref)
  def hasVirtualMethod(ref: String): Boolean =
    hasVirtualMethod(MethodRef.parse(ref, classLoader))

  // def fields: Set[FieldRef]
}
object Instance {
  def of[A <: AnyRef](value: A): Original[A] = Original(value)

  case class Original[A <: AnyRef](value: A) extends Instance[A] {
    require(value != null)

    def duplicate[B >: A <: AnyRef: ClassTag](): Duplicate[B] = {
      val baseClass = implicitly[ClassTag[B]].runtimeClass.asInstanceOf[Class[B]]
      val baseRef = ClassRef.of(baseClass)
      val thisRef = baseRef.someSubclassRef(baseRef.classLoader)
      Duplicate[B](this, thisRef, virtualJMethods.flatMap { case (mref, jm) =>
        if(ClassRef.of(jm.getDeclaringClass) < baseRef)
          methodBody(mref).map { b =>
            import Bytecode._
          // TODO: rewriteClassRef
            mref -> b.rewrite {
              // TODO: invokespecial, fields
              case iv @ invokevirtual(cref, mref) if cref < baseRef =>
                invokevirtual(thisRef, mref)
              case bc @ getfield(cref, fref) if cref < baseRef =>
                getfield(thisRef, fref)
            }
          }
        else
          None
      }.toMap)
    }

    override def methodBody(ref: MethodRef): Option[MethodBody] =
      MethodBody.parse(virtualJMethods(ref))

    override def methodBody(cr: ClassRef, mr: MethodRef) = MethodBody.parse(allJMethods(cr -> mr))

    override def classLoader = value.getClass.getClassLoader

    override lazy val virtualMethods: Map[MethodRef, MethodAttribute] =
      virtualJMethods.map { case (r, m) => r -> MethodAttribute.from(m) }

    override lazy val allMethods: Map[(ClassRef, MethodRef), MethodAttribute] =
      allJMethods.map { case (k, m) => k -> MethodAttribute.from(m) }

    private[this] lazy val jClass = value.getClass

    // TODO: default interface method
    private[this] lazy val allJMethods: Map[(ClassRef, MethodRef), JMethod] =
      supers(jClass)
        .flatMap(_.getDeclaredMethods)
        .map { m => (ClassRef.of(m.getDeclaringClass) -> MethodRef.from(m)) -> m }
        .toMap

    // TODO: default interface method
    private[this] lazy val virtualJMethods: Map[MethodRef, JMethod] =
      supers(jClass)
        .reverse
        .flatMap(_.getDeclaredMethods)
        .filterNot { m => MethodAttribute.Private.enabled(m.getModifiers) }
        .foldLeft(Map.empty[MethodRef, JMethod]) { case (map, m) =>
          map + (MethodRef.from(m) -> m)
        }

    private[this] def supers(klass: Class[_]): Seq[Class[_]] =
      klass +: Option(klass.getSuperclass).toSeq.flatMap(supers)
  }

  case class Duplicate[A <: AnyRef](
    orig: Original[_ <: A],
    thisRef: ClassRef.SomeRef,
    methodBodies: Map[MethodRef, MethodBody]
  ) extends Instance[A] {
    require(orig != null)

    override def methodBody(ref: MethodRef): Option[MethodBody] =
      methodBodies.get(ref) orElse orig.methodBody(ref)

    override def methodBody(cr: ClassRef, mr: MethodRef) = ???

    override def classLoader = orig.classLoader

    override def virtualMethods = orig.virtualMethods
    override def allMethods = ???

    lazy val visibleSuperConstructors: Set[MethodDescriptor] =
      thisRef.superClass.getDeclaredConstructors()
        .filterNot { m => (m.getModifiers & Modifier.PRIVATE) == Modifier.PRIVATE }
        .map(MethodDescriptor.from)
        .toSet

    lazy val setterOnlyBaseConstructors: Map[MethodDescriptor, Map[FieldRef, Int]] = {
      // TODO: accept const field
      visibleSuperConstructors.flatMap { d =>
        methodBody(MethodRef("<init>", d)).filter { c =>
          import Bytecode._
          c.bytecode.forall {
            case bc: Shuffle => true
            case bc: Jump => true
            case bc: Return => true
            case bc: ConstX => false
            // case pf @ putfield(cref, fref) => c.initialFrame.locals.values.filterNot(_._1 == 0).values.map(_._1).contains(c.binding(pf.in))
            case bc => throw new NotImplementedError(bc.toString)
          }
        }.map { c =>
          val argFields = ???

          d -> argFields
        }
      }.toMap
    }

    def methodModified(m: MethodRef): Boolean =
      methodBodies.contains(m)

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

      val classLoader = thisRef.classLoader
      val className = makeUniqueName(classLoader, thisRef.superClass)
      val baseRef = ClassRef.of(thisRef.superClass)
      val classRef = ClassRef.Concrete(className, classLoader)

      val classPool = new ClassPool(null)
      Instance.findMaterializedClasses(classLoader).foreach { case (name, bytes) =>
        classPool.appendClassPath(new ByteArrayClassPath(name, bytes))
      }
      classPool.appendClassPath(new ClassClassPath(thisRef.superClass))

      val ctBase = classPool.get(thisRef.superClass.getName)

      val klass = classPool.makeClass(className, ctBase)
      val constPool = klass.getClassFile.getConstPool
      val ctObject = classPool.get("java.lang.Object")
      def concrete(r: ClassRef): ClassRef.Concrete = r match {
        case c: ClassRef.Concrete => c
        case unk => throw new AssertionError(s"Not concrete class reference: ${unk}")
      }
      virtualMethods.keys.filter { m => methodModified(m) } foreach { ref =>
        methodBody(ref).fold(throw new AssertionError) { body =>
          val out = new JABytecode(constPool, 0, 0)
          val jumps = mutable.HashMap.empty[Int, (Int, JumpTarget)] // jump operand address -> (insn addr -> target)
          val addrs = mutable.HashMap.empty[Bytecode.Label, Int]
          import Bytecode._
          body.rewrite {
            case invokevirtual(thisRef, m) =>
              invokevirtual(classRef, m)
            case getfield(thisRef, f) =>
              getfield(classRef, f)
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
          val ca = out.toCodeAttribute
          val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
          minfo.setCodeAttribute(ca)
          val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, minfo)
          ca.setAttribute(sm)
          klass.getClassFile.addMethod(minfo)
        }
      }

      // fields.filter { f =>
      //   fieldModified(f) || fieldDefinitionClass(f).getOrElse(classRef) < classRef
      // } foreach { f =>
      // }

      val ctor = new CtConstructor(Array.empty, klass)
      ctor.setBody("super();")
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

      if((thisRef.superClass.getModifiers & Modifier.FINAL) == Modifier.FINAL)
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
}

