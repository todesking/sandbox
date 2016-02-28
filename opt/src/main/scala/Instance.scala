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
  def methodBody(ref: MethodRef): Option[MethodBody]
  def methodBody(classRef: ClassRef, methodRef: MethodRef): Option[MethodBody]

  def classLoader: ClassLoader

  def virtualMethods: Map[MethodRef, MethodAttribute]
  def allMethods: Map[(ClassRef, MethodRef), MethodAttribute]

  def allFields: Map[(ClassRef, FieldRef), Field]

  def hasVirtualMethod(ref: MethodRef): Boolean =
    virtualMethods.contains(ref)
  def hasVirtualMethod(ref: String): Boolean =
    hasVirtualMethod(MethodRef.parse(ref, classLoader))
}
object Instance {
  def of[A <: AnyRef](value: A): Original[A] = Original(value)

  case class Original[A <: AnyRef](value: A) extends Instance[A] {
    require(value != null)

    // TODO: make this REAL unique
    private[this] def makeUniqueField(cr: ClassRef, fr: FieldRef): FieldRef =
      FieldRef(s"${cr.pretty.replaceAll("[^A-Za-z0-9]", "_")}_${fr.name}", fr.descriptor)

    def duplicate[B >: A <: AnyRef: ClassTag](): Duplicate[B] = {
      val baseClass = implicitly[ClassTag[B]].runtimeClass.asInstanceOf[Class[B]]
      val baseRef = ClassRef.of(baseClass)
      val thisRef = baseRef.someSubclassRef(baseRef.classLoader)
      val fieldMappings: Map[(ClassRef, FieldRef), FieldRef] =
        allFields
          .keys
          .filter { case (c, f) => c < baseRef }
          .map { case (c, f) => (c, f) -> makeUniqueField(c, f) }
          .toMap
      val fieldValues =
        allJFields
          .filterNot { case (k, v) => fieldMappings.contains(k) }
          .map { case (k, jf) => k -> Field.from(jf, value) }
      Duplicate[B](this, thisRef, virtualJMethods.flatMap { case (mref, jm) =>
        if(ClassRef.of(jm.getDeclaringClass) < baseRef)
          methodBody(mref).map { b =>
            import Bytecode._
            // TODO: make MethodBody.rewriteClassRef
            // TODO: make MethodBody.rewriteFieldRef
            Some(
              mref -> b.rewrite {
                // TODO: invokespecial, fields
                case iv @ invokevirtual(cref, mref) if cref < baseRef =>
                  invokevirtual(thisRef, mref)
                case bc @ getfield(cref, fref) if fieldMappings.contains(cref -> fref) =>
                  getfield(thisRef, fieldMappings(cref -> fref))
              }
            )
          } getOrElse { throw new RuntimeException(s"Method ${jm.getDeclaringClass.getName}.${mref.str} is abstract or native") }
        else
          None
        }.toMap,
        fieldValues,
        allFields.flatMap { case (cf, field) =>
          fieldMappings.get(cf).map { f =>
            val jf = allJFields(cf)
            val field = Field.from(jf, value)
            f -> field.copy(attribute = field.attribute | FieldAttribute.Private)
          }
        }.toMap
      )
    }

    override def methodBody(ref: MethodRef): Option[MethodBody] =
      MethodBody.parse(virtualJMethods(ref))

    override def methodBody(cr: ClassRef, mr: MethodRef) = MethodBody.parse(allJMethods(cr -> mr))

    override def classLoader = value.getClass.getClassLoader

    override lazy val virtualMethods: Map[MethodRef, MethodAttribute] =
      virtualJMethods.map { case (r, m) => r -> MethodAttribute.from(m) }

    override lazy val allMethods: Map[(ClassRef, MethodRef), MethodAttribute] =
      allJMethods.map { case (k, m) => k -> MethodAttribute.from(m) }

    override lazy val allFields: Map[(ClassRef, FieldRef), Field] =
      allJFields.map { case ((cr, fr), f) => (cr -> fr) -> Field.from(f, value) }

    private[this] lazy val jClass = value.getClass
    private[this] lazy val virtualJMethods = Util.virtualJMethods(jClass)
    private[this] lazy val allJMethods = Util.allJMethods(jClass)
    private[this] lazy val allJFields = Util.allJFields(jClass)
  }

  // TODO: Do we really need ClassRef.SomeRef ?????
  case class Duplicate[A <: AnyRef](
    orig: Original[_ <: A],
    thisRef: ClassRef.SomeRef,
    methodBodies: Map[MethodRef, MethodBody],
    fieldValues: Map[(ClassRef, FieldRef), Field], // super class field values
    privateFields: Map[FieldRef, Field]
  ) extends Instance[A] {
    require(orig != null)
    require(privateFields.forall { case (ref, field) => field.attribute.has(FieldAttribute.Private) })

    override def methodBody(ref: MethodRef): Option[MethodBody] =
      methodBodies.get(ref) orElse orig.methodBody(ref)

    override def methodBody(cr: ClassRef, mr: MethodRef) = ???

    override def classLoader = orig.classLoader

    override def virtualMethods = orig.virtualMethods
    override def allMethods = ???
    override lazy val allFields: Map[(ClassRef, FieldRef), Field] =
      Util.allJFields(thisRef.superClass).map { case (k, jf) =>
        k -> Field.from(jf, orig.value)
      } ++ privateFields.map { case (r, f) => (thisRef -> r) -> f }

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
      import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod, CtField, CtConstructor, ByteArrayClassPath }
      import javassist.bytecode.{ Bytecode => JABytecode, MethodInfo }

      import Javassist.ctClass

      validate()

      val superClass = thisRef.superClass
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
      import Bytecode._
      virtualMethods.keys.filter { m => methodModified(m) } foreach { ref =>
        methodBody(ref).map {
          _.rewrite {
            case getfield(thisRef, f) =>
              getfield(classRef, f)
            case invokevirtual(thisRef, m) =>
              invokevirtual(classRef, m)
          }
        }.fold(throw new AssertionError) { body =>
          val codeAttribute = Javassist.compile(classPool, constPool, body)
          val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
          minfo.setCodeAttribute(codeAttribute)
          val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, minfo)
          codeAttribute.setAttribute(sm)
          klass.getClassFile.addMethod(minfo)
        }
      }

      privateFields.foreach { case (ref, field) =>
        val ctf = new CtField(ctClass(ref.descriptor.typeRef), ref.name, klass)
        ctf.setModifiers(field.attribute.toModifiers)
        klass.addField(ctf)
      }

      val allFieldValues: Seq[(ClassRef, FieldRef, FieldValue)] =
        fieldValues.map { case ((c, r), f) => (c, r, f.value) }.toSeq ++ privateFields.map { case (r, f) => (classRef, r, f.value) }

      def parseAssigns(ctor: Constructor[_]): Option[Map[(ClassRef, FieldRef), Either[Int, Any]]] = {
        Javassist.decompile(ctor).map { b =>
          def isThrowNull(l: Bytecode.Label): Boolean = {
            import Bytecode._
            b.labelToBytecode(l) match {
              case bc @ aconst_null() =>
                isThrowNull(b.fallThroughs(bc.label))
              case athrow() =>
                true
              case other =>
                false
            }
          }
          import Bytecode._
          // TODO: MethodBody.basicBlocks
          b.bytecode.foldLeft(Map.empty[(ClassRef, FieldRef), Either[Int, Any]]) { case (assigns, bc) =>
            bc match {
              case bc: Shuffle => assigns
              case bc: Jump => assigns
              case bc: Return => assigns
              case bc: ConstX => assigns
              case bc @ invokespecial(classRef, methodRef)
              if b.dataValue(bc.receiver).typeRef == TypeRef.This && methodRef.name == "<init>" =>
                val klass = classRef match { case cr @ ClassRef.Concrete(_, _) => cr.loadClass ; case unk => throw new AssertionError(s"${unk}")}
                val ctor = klass.getDeclaredConstructors.find { c => MethodRef.from(c) == methodRef }.get
                // TODO: FIXME: WRONG.
                parseAssigns(ctor).map { as => assigns ++ as } getOrElse { return None }
              case bc @ putfield(classRef, fieldRef) if b.dataValue(bc.target).typeRef == TypeRef.This =>
                assigns + (
                  b.dataValue(bc.value).value.map { v =>
                    (classRef -> fieldRef) -> Right(v) // constant
                  } getOrElse {
                    def argNum(label: DataLabel.Out): Option[Int] = {
                      val index = b.argLabels.indexOf(label)
                      if(index == -1) None else Some(index)
                    }
                    val l = b.dataBinding(bc.value)
                    argNum(l).map { i => (classRef -> fieldRef) -> Left(i) } getOrElse { return None }
                  }
                )
              case athrow() => return None
              case bc @ ifnonnull(target) if isThrowNull(b.fallThroughs(bc.label)) =>
                // TODO: NIMPL
                // TODO: exception handler
                // TODO: mark the value as non-nullable
                // assigns
                return None
              case bc: Branch => return None
              case bc: Procedure => return None
            }
          }
        }
      }

      def mapZip[A, B, C](a: Map[A, B], b: Map[A, C]): (Map[A, (B, C)], Map[A, B], Map[A, C]) = {
        val aOnly = a.keySet -- b.keySet
        val bOnly = b.keySet -- a.keySet
        val common = a.keySet -- aOnly
        Tuple3(
          common.map { k => k -> (a(k) -> b(k)) }.toMap,
          a.filterKeys(aOnly),
          b.filterKeys(bOnly)
        )
      }

      val superCtors: Map[MethodDescriptor, Constructor[_]] = superClass
        .getDeclaredConstructors
        .filterNot { c => MethodAttribute.Private.enabled(c.getModifiers) }
        .map { c => MethodDescriptor.from(c) -> c }
        .toMap

      val ctorAssigns: Map[MethodDescriptor, Map[(ClassRef, FieldRef), Either[Int, Any]]] =
        superCtors
          .mapValues(parseAssigns)
          .collect { case (k, Some(v)) => (k -> v) }

      val (superCtor, assigns) =
        ctorAssigns.find { case (ctor, assigns) =>
          val (common, unkFieldValues, unkAssigns) = mapZip(fieldValues.mapValues(_.value.value), assigns)
          if(unkFieldValues.nonEmpty) {
            throw new RuntimeException(s"Unknown field values: ${unkFieldValues}")
          }
          if(unkAssigns.nonEmpty) {
            throw new RuntimeException(s"Unknown assigns in constructor: ${unkAssigns}")
          }
          common.forall {
            case ((classRef, fieldRef), (v1, Right(v2))) =>
              fieldRef.descriptor.typeRef match {
                case p: TypeRef.Primitive => v1 == v2
                case r: TypeRef.Reference => v1.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef] // TODO: care String
              }
            case ((classRef, fieldRef), (v1, Left(n))) =>
              true
          }
        }.getOrElse {
          throw new RuntimeException(s"Usable constructor not found")
        }

      // TODO: set private field values via ctor
      // TODO: set non-const field values via ctor

      if(superCtor.args.size > 0)
        ???

      val argAssigns = assigns.collect { case (k, Left(i)) => (k -> i) }
      val superCtorArgs: Seq[Int] = Seq.empty

      val privateFieldSeq: Seq[(FieldRef, Field)] = privateFields.toSeq
      val ctorArgs: Seq[(TypeRef.Public, Any)] = privateFieldSeq.map { case (r, f) => (f.descriptor.typeRef -> f.value.value) }

      val fieldAssigns: Map[FieldRef, Int] =
        privateFieldSeq.zipWithIndex.map { case ((fr, f), i) => fr -> (i + 1) }.toMap

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
        isStatic = false,
        descriptor = ctorDescriptor,
        jumpTargets = Map.empty,
        maxLocals = 100,
        maxStackDepth = 100,
        bytecode =
          Seq(
            aload(0),
            // TODO: call with args
            invokespecial(
              ClassRef.of(superClass),
              MethodRef("<init>", MethodDescriptor(TypeRef.Void, Seq.empty)))
          ) ++ fieldAssigns.flatMap { case (fr, i) =>
            import Bytecode._
            Seq(
              aload(0),
              load(fr.descriptor.typeRef, i),
              putfield(classRef, fr)
            )
          }.toSeq ++ Seq(vreturn())
      ))

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

    private[this] def makeUniqueName(cl: ClassLoader, klass: Class[_]): String = {
      // TODO: make this REAL UNIQUE!!!!
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

  object Util {
    // TODO: default interface method
    def allJMethods(jClass: Class[_]): Map[(ClassRef, MethodRef), JMethod] =
      supers(jClass)
        .flatMap(_.getDeclaredMethods)
        .map { m => (ClassRef.of(m.getDeclaringClass) -> MethodRef.from(m)) -> m }
        .toMap

    // TODO: default interface method
    def virtualJMethods(jClass: Class[_]): Map[MethodRef, JMethod] =
      supers(jClass)
        .reverse
        .flatMap(_.getDeclaredMethods)
        .filterNot { m => MethodAttribute.Private.enabled(m.getModifiers) }
        .foldLeft(Map.empty[MethodRef, JMethod]) { case (map, m) =>
          map + (MethodRef.from(m) -> m)
        }

    def allJFields(jClass: Class[_]): Map[(ClassRef, FieldRef), JField] =
      supers(jClass)
        .flatMap(_.getDeclaredFields)
        .map { f => f.setAccessible(true); f } // I believe this it no trigger any bad side-effects
        .map { f => (ClassRef.of(f.getDeclaringClass) -> FieldRef.from(f)) -> f }
        .toMap

    def supers(klass: Class[_]): Seq[Class[_]] =
      klass +: Option(klass.getSuperclass).toSeq.flatMap(supers)
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

