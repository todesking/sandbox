package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.collection.JavaConversions._

import java.lang.reflect.{ Method => JMethod, Field => JField, Modifier, Constructor }

import com.todesking.scalapp.syntax._

sealed abstract class Instance[A <: AnyRef] {
  // TODO: return Success/Abstract/Native/UnSupportedOp
  def methodBody(ref: MethodRef): Option[MethodBody] =
    methodBody(thisRef, ref)

  def methodBody(classRef: ClassRef, methodRef: MethodRef): Option[MethodBody]

  def thisRef: ClassRef

  def methods: Map[(ClassRef, MethodRef), MethodAttribute]

  def fields: Map[(ClassRef, FieldRef), Field]

  def hasVirtualMethod(ref: MethodRef): Boolean =
    methods.exists { case ((c, m), a) => m == ref && a.isVirtual }

  def hasVirtualMethod(ref: String): Boolean =
    hasVirtualMethod(MethodRef.parse(ref, thisRef.classLoader))

  def resolveVirtualMethod(mr: MethodRef): ClassRef

  def materialized: Instance.Original[A]

  def duplicate[B >: A <: AnyRef: ClassTag]: Instance.Duplicate[B]

  def duplicate1: Instance.Duplicate[A]

  def usedMethodsOf(i: Instance[_ <: AnyRef]): Option[Set[(ClassRef, MethodRef)]] = {
    analyzeBytecodes(Set.empty[(ClassRef, MethodRef)]) { (agg, cr, mr, df, bc) =>
      import Bytecode._
      bc match {
        case bc @ invokevirtual(cr, mr) =>
          ifSingleInstance(df, bc.objectref, i).map { mustTheInstance =>
            val vcr = i.resolveVirtualMethod(mr)
            if (mustTheInstance) agg + (cr -> mr)
            else agg
          } orElse { println(s"Ambigious reference: ${cr}.${mr} ${bc} ${df.possibleValues(bc.objectref)}"); None }
        case _ => Some(agg)
      }
    }
  }

  def usedFieldsOf(i: Instance[_ <: AnyRef]): Option[Set[(ClassRef, FieldRef)]] = {
    analyzeBytecodes(Set.empty[(ClassRef, FieldRef)]) { (agg, cr, mr, df, bc) =>
      import Bytecode._
      bc match {
        case bc: FieldAccess =>
          ifSingleInstance(df, bc.objectref, i).map { mustTheInstance =>
            if (mustTheInstance) agg + (bc.classRef -> bc.fieldRef)
            else agg
          } orElse { println(s"Ambigious reference: ${cr}.${mr} ${bc} ${df.possibleValues(bc.objectref)}"); None }
        case _ => Some(agg)
      }
    }
  }

  // Some(true): data has single value that point the instance
  // Some(false): data is not point the instance
  // None: not sure
  private[this] def ifSingleInstance(df: MethodBody.DataFlow, l: DataLabel, i: Instance[_ <: AnyRef]): Option[Boolean] =
    df.onlyValue(l).map(_.isInstance(i)) orElse {
      if (df.possibleValues(l).exists(_.isInstance(i))) None
      else Some(false)
    }

  def analyzeMethods[B](initial: B)(analyze: (B, ClassRef, MethodRef, MethodBody.DataFlow) => Option[B]): Option[B] = {
    val ms = methods.filterNot { case (k, attrs) => attrs.isAbstract }.keys.toSeq.filterNot { case (cr, mr) => cr == ClassRef.Object }
    breakableFoldLeft(initial)(ms) {
      case (agg, (cr, mr)) =>
        methodBody(cr, mr).orElse { println(s"Method cant decompile: ${cr}.${mr}"); None }
          .flatMap { body => analyze(agg, cr, mr, body.dataflow(this)) }
    }
  }

  def analyzeBytecodes[B](initial: B)(analyze: (B, ClassRef, MethodRef, MethodBody.DataFlow, Bytecode) => Option[B]): Option[B] =
    analyzeMethods(initial) {
      case (mAgg, cr, mr, df) =>
        breakableFoldLeft(mAgg)(df.body.bytecode) {
          case (agg, bc) =>
            analyze(agg, cr, mr, df, bc)
        }
    }

  private[this] def breakableFoldLeft[X, Y](initial: Y)(seq: Seq[X])(f: (Y, X) => Option[Y]): Option[Y] = {
    def loop(rest: Seq[X], agg: Y): Option[Y] =
      rest match {
        case Seq() => Some(agg)
        case Seq(x, r @ _*) => f(agg, x).flatMap { ret => loop(r, ret) }
      }
    loop(seq, initial)
  }

  override final def hashCode() = System.identityHashCode(this)
  override final def equals(that: Any) = that match { case that: AnyRef => this eq that; case _ => false }
}
object Instance {
  def of[A <: AnyRef](value: A): Original[A] = Original(value)

  case class Original[A <: AnyRef](value: A) extends Instance[A] {
    require(value != null)

    override def materialized = this

    override val thisRef = ClassRef.of(jClass)

    override def resolveVirtualMethod(mr: MethodRef): ClassRef =
      Reflect.resolveVirtualMethod(jClass, mr)

    override def duplicate[B >: A <: AnyRef: ClassTag]: Duplicate[B] =
      duplicate1.duplicate[B]

    override def duplicate1: Duplicate[A] =
      Duplicate[A](
        this,
        thisRef.extend(thisRef.classLoader),
        methods,
        Map.empty,
        fields,
        Map.empty
      )

    override def methodBody(ref: MethodRef): Option[MethodBody] =
      MethodBody.parse(virtualJMethods(ref))

    override def methodBody(cr: ClassRef, mr: MethodRef) = MethodBody.parse(allJMethods(cr -> mr))

    override lazy val methods: Map[(ClassRef, MethodRef), MethodAttribute] =
      allJMethods.map { case (k, m) => k -> MethodAttribute.from(m) }

    override lazy val fields: Map[(ClassRef, FieldRef), Field] =
      allJFields.map { case ((cr, fr), f) => (cr -> fr) -> Field.from(f, value) }

    private[this] lazy val jClass = value.getClass
    private[this] lazy val virtualJMethods = Reflect.virtualJMethods(jClass)
    private[this] lazy val allJMethods = Reflect.allJMethods(jClass)
    private[this] lazy val allJFields = Reflect.allJFields(jClass)
  }

  case class Duplicate[A <: AnyRef](
      orig: Original[_ <: A],
      override val thisRef: ClassRef.Extend,
      superMethods: Map[(ClassRef, MethodRef), MethodAttribute],
      thisMethods: Map[MethodRef, MethodBody],
      superFields: Map[(ClassRef, FieldRef), Field], // super class field values
      thisFields: Map[FieldRef, Field]
  ) extends Instance[A] {
    def pretty: String = s"""class ${thisRef}
new/overriden methods:
${
      thisMethods.map {
        case (mr, body) =>
          s"""def ${mr} ${body.attribute}
${body.pretty}
"""
      }.mkString("\n")
    }
new fields:
${
      thisFields.map {
        case (fr, field) => fr.toString
      }.mkString("\n")
    }
"""

    def addMethod(mr: MethodRef, body: MethodBody): Duplicate[A] = {
      require(mr.descriptor == body.descriptor)
      copy(thisMethods = thisMethods + (mr -> body))
    }

    def addField(fr: FieldRef, field: Field): Duplicate[A] = {
      copy(thisFields = thisFields + (fr -> field))
    }

    override def resolveVirtualMethod(mr: MethodRef): ClassRef = {
      thisMethods.get(mr).map { body =>
        if (body.attribute.isVirtual) thisRef
        else throw new IllegalArgumentException(s"Not virtual: ${mr} ${body.attribute}")
      } getOrElse {
        orig.resolveVirtualMethod(mr)
      }
    }

    override def duplicate1 =
      rewriteThisRef(thisRef.anotherUniqueName)

    override def duplicate[B >: A <: AnyRef: ClassTag]: Duplicate[B] = {
      val newSuperRef = ClassRef.of(implicitly[ClassTag[B]].runtimeClass)
      val newRef = newSuperRef.extend(thisRef.anotherUniqueName.name, thisRef.classLoader)
      val fieldMappings: Map[(ClassRef, FieldRef), FieldRef] =
        superFields
          .keys
          .filter { case (c, f) => c < newSuperRef }
          .map { case (c, f) => (c, f) -> f.anotherUniqueName(c.name, f.name) }
          .toMap
      val newThisFields =
        superFields
          .flatMap {
            case (cf, field) =>
              fieldMappings.get(cf).map { fr =>
                fr -> field.copy(attribute = field.attribute | FieldAttribute.Private)
              }
          }
      // TODO: support super methods
      val newThisMethods =
        superMethods
          .filter { case ((cr, mr), ma) => cr < newSuperRef }
          .map {
            case ((cr, mr), ma) =>
              import Bytecode._
              mr -> orig.methodBody(cr, mr).map { body =>
                body.rewrite {
                  case iv @ invokevirtual(cref, mref) if cref < newSuperRef =>
                    invokevirtual(newRef, mref)
                  case bc @ getfield(cref, fref) if fieldMappings.contains(cref -> fref) =>
                    getfield(newRef, fieldMappings(cref -> fref))
                  case bc @ putfield(cref, fref) if fieldMappings.contains(cref -> fref) =>
                    putfield(newRef, fieldMappings(cref -> fref))
                }
              }.getOrElse {
                throw new RuntimeException(s"Method ${cr.pretty}.${mr.str} can't rewrite: It's abstract or native")
              }
          }
      copy[B](
        superMethods = superMethods.filterNot { case ((cr, mr), ma) => cr < newSuperRef },
        superFields = superFields.filterNot { case ((cr, fr), f) => cr < newSuperRef },
        thisMethods = thisMethods ++ newThisMethods,
        thisFields = thisFields ++ newThisFields
      ).rewriteThisRef(newRef)
    }

    // TODO: should we replace thisRef in method/field signature?
    // TODO: should we replace only if objectref == this ?
    def rewriteThisRef(newRef: ClassRef.Extend): Duplicate[A] =
      copy(
        thisRef = newRef,
        thisMethods = thisMethods.map { case (ref, body) => ref -> body.rewriteClassRef(thisRef, newRef) }
      )

    override def methodBody(cr: ClassRef, mr: MethodRef) =
      if (cr == thisRef) thisMethods.get(mr)
      else if (thisRef < cr) orig.methodBody(cr, mr)
      else throw new IllegalArgumentException(s"Method not found: ${cr.pretty}.${mr.str}")

    override def methods =
      superMethods ++ thisMethods.map { case (k, v) => (thisRef -> k) -> v.attribute }

    override lazy val fields: Map[(ClassRef, FieldRef), Field] =
      superFields ++ thisFields.map { case (fref, f) => ((thisRef -> fref) -> f) }

    override lazy val materialized: Original[A] = {
      import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod, CtField, CtConstructor, ByteArrayClassPath }
      import javassist.bytecode.{ Bytecode => JABytecode, MethodInfo }

      import Javassist.ctClass

      validate()

      val superClass = thisRef.superClass
      val classLoader = thisRef.classLoader
      val baseRef = ClassRef.of(thisRef.superClass)

      val classPool = new ClassPool(null)
      Instance.findMaterializedClasses(classLoader).foreach {
        case (name, bytes) =>
          classPool.appendClassPath(new ByteArrayClassPath(name, bytes))
      }
      classPool.appendClassPath(new ClassClassPath(thisRef.superClass))

      val ctBase = classPool.get(thisRef.superClass.getName)

      val klass = classPool.makeClass(thisRef.name, ctBase)
      val constPool = klass.getClassFile.getConstPool
      val ctObject = classPool.get("java.lang.Object")
      import Bytecode._
      thisMethods
        .foreach {
          case (ref, body) =>
            val codeAttribute = Javassist.compile(classPool, constPool, body.dataflow(this))
            val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
            minfo.setCodeAttribute(codeAttribute)
            val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, minfo)
            codeAttribute.setAttribute(sm)
            klass.getClassFile.addMethod(minfo)
        }

      thisFields.foreach {
        case (ref, field) =>
          val ctf = new CtField(ctClass(ref.descriptor.typeRef), ref.name, klass)
          ctf.setModifiers(field.attribute.toInt)
          klass.addField(ctf)
      }

      val (superCtor, assigns) =
        Analyze.findSetterConstructor(this, superClass, superFields) getOrElse {
          throw new RuntimeException(s"Usable constructor not found")
        }

      // TODO: set private field values via ctor
      // TODO: set non-const field values via ctor

      if (superCtor.args.size > 0)
        ???

      val argAssigns = assigns.collect { case (k, Left(i)) => (k -> i) }
      val superCtorArgs: Seq[Int] = Seq.empty

      val thisFieldsSeq: Seq[(FieldRef, Field)] = thisFields.toSeq
      val ctorArgs: Seq[(TypeRef.Public, Any)] = thisFieldsSeq.map { case (r, f) => (f.descriptor.typeRef -> f.data.concreteValue) }

      val fieldAssigns: Map[FieldRef, Int] =
        thisFieldsSeq.zipWithIndex.map { case ((fr, f), i) => fr -> (i + 1) }.toMap

      val ctorDescriptor = MethodDescriptor(TypeRef.Void, ctorArgs.map(_._1))

      val ctor = new CtConstructor(ctorArgs.map(_._1).map(ctClass).toArray, klass)
      klass.addConstructor(ctor)

      val ctorMethodInfo =
        klass
          .getClassFile
          .getMethods
          .map(_.asInstanceOf[MethodInfo])
          .find(_.getName == "<init>")
          .get

      val ctorCA = Javassist.compile(classPool, constPool, MethodBody(
        descriptor = ctorDescriptor,
        MethodAttribute.from(ctorMethodInfo.getAccessFlags),
        jumpTargets = Map.empty,
        bytecode =
          Seq(
            aload(0),
            // TODO: call with args
            invokespecial(
              ClassRef.of(superClass),
              MethodRef("<init>", MethodDescriptor(TypeRef.Void, Seq.empty))
            )
          ) ++ fieldAssigns.flatMap {
              case (fr, i) =>
                import Bytecode._
                Seq(
                  aload(0),
                  load(fr.descriptor.typeRef, i),
                  putfield(thisRef, fr)
                )
            }.toSeq ++ Seq(vreturn())
      ).dataflow(this))

      ctorMethodInfo.setCodeAttribute(ctorCA)
      val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, ctorMethodInfo)
      ctorCA.setAttribute(sm)

      val concreteClass = klass.toClass(classLoader, null)
      val value = concreteClass
        .getDeclaredConstructor(ctorArgs.map(_._1.javaClass).toArray: _*)
        .newInstance(ctorArgs.map(_._2.asInstanceOf[Object]).toArray: _*)
        .asInstanceOf[A]
      val bytes = klass.toBytecode
      Instance.registerMaterialized(classLoader, klass.getName, bytes)
      Instance.of(value)
    }

    private[this] def validate(): Unit = {
      def fail(msg: String) =
        throw new IllegalStateException(msg)

      if ((thisRef.superClass.getModifiers & Modifier.FINAL) == Modifier.FINAL)
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
    if (materializedClasses.contains(cl -> name))
      throw new IllegalArgumentException(s"${name} is already defined in ${cl}")
    materializedClasses(cl -> name) = bytes
  }
  // TODO: Resolve name conflict
  def findMaterializedClasses(cl: ClassLoader): Seq[(String, Array[Byte])] = synchronized {
    if (cl == null) {
      Seq.empty
    } else {
      materializedClasses.collect { case ((l, n), b) if l == cl => (n -> b) }.toSeq ++
        findMaterializedClasses(cl.getParent)
    }
  }
}

