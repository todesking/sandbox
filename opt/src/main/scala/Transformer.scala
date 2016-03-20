package com.todesking.hoge

import scala.language.existentials

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

trait Transformer { self =>
  def name: String
  def params: Map[String, String] = Map.empty

  def apply[A <: AnyRef](orig: Instance[A], el: EventLogger): Try[Instance.Duplicate[A]] =
    try {
      el.enterTransformer(this, orig) { el => Success(apply0(orig, el)) }
    } catch {
      case e: UnveilException =>
        Failure(e)
    }
  protected[this] def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger): Instance.Duplicate[A]

  def andThen(next: Transformer): Transformer =
    new Transformer {
      override def name = s"${self.name} >>> ${next.name}"
      override def apply[A <: AnyRef](orig: Instance[A], el: EventLogger): Try[Instance.Duplicate[A]] = {
        el.enterTransformer(this, orig) { el =>
          self.apply(orig, el).flatMap { i2 => next.apply(i2, el) }
        }
      }
      override def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger) = throw new AssertionError()
    }
}
object Transformer {
  def newEventLogger(): EventLogger =
    new EventLogger


  object fieldFusion extends Transformer {
    override def name = "fieldFusion"
    override def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
      val i = orig.duplicate1
      val targetFields =
        i.fields
          .filter { case ((cr, fr), f) => f.attribute.isFinal && (cr == i.thisRef || !f.attribute.isPrivate) }
          .filter { case (_, f) =>
            f.data match {
              case Data.Reference(typeRef, instance) =>
                instance.fields.forall { case (_, f) => f.attribute.isFinal }
              case _ =>
                false
            }
          }
      el.logCFields("target fields", targetFields.keys)
      targetFields.keys
        .foldLeft(i) {
          case (i, (cr, fr)) =>
            fieldFusion1(cr, fr).apply(i, el) getOrElse i
        }
    }
  }

  // TODO: lowerMembers
  object lowerPrivateFields extends Transformer {
    override def name = "lowerPrivateFields"
    override def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
      val dupInstance = orig.duplicate1
      // TODO: support invokespecial methods
      val entryMethods =
        dupInstance.virtualMethods
          .filter { case (mr, a) =>
            dupInstance.resolveVirtualMethod(mr) != ClassRef.Object && !a.isFinal
          }.map { case (mr, a) =>
            mr -> dupInstance.dataflow(mr)
          }.filter { case (mr, df) =>
            // invokespecial need special handling
            df.body.bytecode.forall {
              case bc @ Bytecode.invokespecial(_, _)
              if df.mustInstance(bc.objectref, dupInstance) => false
              case _ => true
            }
          }.filter { case (mr, df) =>
            df.usedFieldsOf(dupInstance).forall { case (cr, mr) =>
              val attr = dupInstance.fields(cr -> mr).attribute
              cr != dupInstance.thisRef && attr.isPrivate
            }
          }
      el.logMethods("lowered methods", entryMethods.keys)
      val copyFields: Map[(ClassRef, FieldRef), (FieldRef, Field)] =
        entryMethods.flatMap {
          case (mr, df) =>
            df.usedFieldsOf(dupInstance).filter {
              case (cr, fr) => cr != dupInstance.thisRef && dupInstance.fields(cr -> fr).attribute.isPrivateFinal
            }
        }.map { case (cr, fr) =>
          val f = dupInstance.fields(cr -> fr)
          (cr -> fr) -> (fr.anotherUniqueName(fr.name) -> f.copy(classRef = dupInstance.thisRef))
        }
      el.logCFields("lowered fields", copyFields.keys)
      val overridenMethods =
        entryMethods.map {
          case (mr, df) =>
            import Bytecode._
            mr -> df.body.rewrite {
              case bc: InstanceFieldAccess if df.mustInstance(bc.objectref, dupInstance) && copyFields.contains(bc.classRef -> bc.fieldRef) =>
                val (fr, _) = copyFields(bc.classRef -> bc.fieldRef)
                bc.rewriteClassRef(dupInstance.thisRef).rewriteFieldRef(fr)
            }
        }
      dupInstance
        .addFields(copyFields.map { case (_, (newFr, f)) => newFr -> f })
        .addMethods(overridenMethods)
    }
  }

  // TODO: support instance-stateful fields(need leakage detection)
  // TODO: support mutable fields(if fref eq original then optimized else original)
  def fieldFusion1(classRef0: ClassRef, fieldRef: FieldRef): Transformer = new Transformer {
    override def name = s"fieldFusion1"
    override def params = Map("field class" -> s"$classRef0", "field" -> s"$fieldRef")
    override def apply0[A <: AnyRef](instance: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
      val dupInstance = instance.duplicate1
      val classRef = if(classRef0 == instance.thisRef) dupInstance.thisRef else classRef0
      dupInstance.fields.get(classRef, fieldRef).fold {
        throw new FieldAnalyzeException(classRef, fieldRef, s"Not found")
      } { field =>
        if (!field.isFinal)
          throw new FieldTransformException(classRef, fieldRef, s"Not final")
        if(classRef > dupInstance.thisRef && field.attribute.isPrivate)
          throw new FieldTransformException(classRef, fieldRef, s"Inaccessible from subclass")
        field.data match {
          case Data.Reference(t, fieldInstance) =>
            fuse(dupInstance, fieldInstance, classRef, el)
          case other =>
            throw new FieldTransformException(classRef, fieldRef, s"Field can't fusable: ${other}")
        }
      }
    }
    def fuse[A <: AnyRef](dupInstance: Instance.Duplicate[A], fieldInstance: Instance[_ <: AnyRef], classRef: ClassRef, el: EventLogger): Instance.Duplicate[A] = {
      if (!fieldInstance.fields.values.forall(_.attribute.isFinal))
        throw new FieldTransformException(classRef, fieldRef, s"Can't fuse instance-stateful field")

      val usedMethods = dupInstance.usedMethodsOf(fieldInstance) ++ fieldInstance.usedMethodsOf(fieldInstance)
      el.logCMethods("used methods of the field", usedMethods)

      val usedFields = dupInstance.usedFieldsOf(fieldInstance) ++ fieldInstance.usedFieldsOf(fieldInstance)
      el.logCFields("used fields of the field", usedFields)

      val methodRenaming =
        usedMethods.map { case (cr, mr) => (cr -> mr) -> mr.anotherUniqueName(fieldRef.name, mr.name) }.toMap
      val fieldRenaming =
        usedFields.map { case (cr, fr) => (cr -> fr) -> fr.anotherUniqueName(fieldRef.name, fr.name) }.toMap

      val targetMethods =
        (dupInstance.thisMethods).collect {
          case (mr, a)
          if dupInstance.dataflow(mr).usedFieldsOf(dupInstance).contains(classRef -> fieldRef) =>
            (dupInstance.thisRef -> mr) -> dupInstance.methodBody(mr)
        }.toMap
      el.logCMethods("rewrite target methods", targetMethods.keys)

      def fusedMemberAccessRewriter(methodRef: MethodRef, df: DataFlow): PartialFunction[Bytecode, Seq[Bytecode]] = {
        import Bytecode._
        {
          case bc: InstanceFieldAccess if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
            Seq(bc.rewriteClassRef(dupInstance.thisRef).rewriteFieldRef(fieldRenaming(bc.classRef -> bc.fieldRef)))
          // TODO: unsupport invokespecial
          case bc @ invokespecial(cr, mr) if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
            Seq(bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(cr -> mr)))
          case bc @ invokevirtual(cr, mr) if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
            val definedCR = fieldInstance.resolveVirtualMethod(mr)
            Seq(bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(definedCR -> mr)))
          case bc @ areturn() if df.mustInstance(bc.objectref, fieldInstance) =>
            Seq(getfield(classRef, fieldRef), areturn())
          case bc if bc.inputs.exists { in => df.mustInstance(in, fieldInstance) } =>
            throw new TransformException(s"$methodRef: not supported yet: ${bc}")
        }
      }

      val fusedMethods =
        methodRenaming.map {
          case ((fCr, fMr), newMR) =>
            val b = fieldInstance.methodBody(fCr, fMr)
            val df = b.dataflow(fieldInstance)
            import Bytecode._
            newMR -> b.rewrite_*(fusedMemberAccessRewriter(fMr, df))
        }
      el.logMethods("fused methods", fusedMethods.keys)

      val additionalFields = fieldRenaming.map { case ((cr, fr), newRef) => newRef -> fieldInstance.fields(cr -> fr) }
      el.logFields("fused fields", additionalFields.keys)

      val withNewFields = dupInstance.addFields(additionalFields)

      def thisMethods =
        targetMethods.map {
          case ((cr, mr), body) =>
            val df = body.dataflow(withNewFields)
            import Bytecode._
            mr -> body.rewrite_*(({
              case bc @ getfield(cr, fr) if cr == classRef && fr == fieldRef && df.mustInstance(bc.objectref, withNewFields, mr, bc) =>
                Seq(nop())
            }: PartialFunction[Bytecode, Seq[Bytecode]]).orElse(fusedMemberAccessRewriter(mr, df)))
        }

      def allMethods = fusedMethods ++ thisMethods
      assert(allMethods.size == fusedMethods.size + thisMethods.size)

      withNewFields.addMethods(allMethods)
    }

    case class FuseResult(
      rewrittenMethods: Map[(ClassRef, MethodRef), MethodBody],
      newMethods: Map[MethodRef, MethodBody],
      newFields: Map[FieldRef, Field]
    )
    // TODO: prevent inf loop
    private[this] def fuse(
      memberPrefix: String,
      self: Instance[_ <: AnyRef],
      thisRef: ClassRef,
      methods: Set[(ClassRef, MethodRef)],
      el: EventLogger
    ): FuseResult = {
      val dfs = methods.toSeq.map { case (cr, mr) => (cr -> mr) -> self.dataflow(cr, mr) }.toMap
      val usedFields: Set[(ClassRef, FieldRef)] =
        dfs.values
          .map(_.usedFieldsOf(self))
          .reduce(_ ++ _)
          .filter { case (cr, fr) => self.fields(cr -> fr).attribute.isFinal }

      val blankInstance = Instance.of(new AnyRef).duplicate1

      val (rewrittenMethods, newMethods, newFields) =
        usedFields.foldLeft(
          (dfs.mapValues(_.body), Map.empty[MethodRef, MethodBody], Map.empty[FieldRef, Field])
        ) { case ((methods, newMethods, newFields), (cr, fr)) =>
          el.enterField(cr, fr) { el =>
            self.fields(cr -> fr).data match {
               case Data.Reference(t, fieldInstance) =>
                 // TODO: add dependent methods
                 val usedMethods = dfs.values.map(_.usedMethodsOf(fieldInstance)).reduce(_ ++ _)
                 val fused1 = fuse(memberPrefix + fr.name + "__", fieldInstance, thisRef, usedMethods, el)
                 val methodRenaming = usedMethods.map { case k @ (cr, mr) => k -> mr.anotherUniqueName(memberPrefix, mr.name) }.toMap
                 val fieldRenaming = fieldInstance.fields.keys.map { case k @ (cr, fr) => k -> fr.anotherUniqueName(memberPrefix, fr.name) }.toMap
                 val newFields1 = fieldRenaming.map { case((cr, fr), nfr) => nfr -> fieldInstance.fields(cr, fr) }
                 val newMethods1 =
                   usedMethods
                     .toIterable
                     .map { case k @ (cr, mr) => k -> fieldInstance.dataflow(cr, mr) }
                     .toMap
                     .map { case ((cr, mr), df) =>
                       methodRenaming(cr -> mr) -> df.body.rewrite {
                         case bc: Bytecode.InvokeInstanceMethod
                         if df.mustThis(bc.objectref) =>
                           bc.rewriteMethodRef(thisRef, methodRenaming(bc.classRef, bc.methodRef))
                         case bc: Bytecode.InstanceFieldAccess
                         if df.mustThis(bc.objectref) =>
                           bc.rewriteFieldRef(thisRef, fieldRenaming(bc.classRef, bc.fieldRef))
                       }
                     }
                  val dummyInstance = blankInstance.addFields(newFields ++ newFields1)
                  val rewrittenMethods =
                    methods.mapValues { b =>
                      val df = b.dataflow(dummyInstance)
                      b.rewrite {
                        case bc: Bytecode.InvokeInstanceMethod
                        if df.mustThis(bc.objectref) =>
                          bc.rewriteMethodRef(thisRef, methodRenaming(bc.classRef, bc.methodRef))
                        case bc: Bytecode.InstanceFieldAccess
                        if df.mustThis(bc.objectref) =>
                          bc.rewriteFieldRef(thisRef, fieldRenaming(bc.classRef, bc.fieldRef))
                      }
                    }
                  (rewrittenMethods, newMethods ++ newMethods1, newFields ++ newFields1)
               case _ =>
                 (methods, newMethods, newFields)
            }
          }
        }
      FuseResult(rewrittenMethods, newMethods, newFields)
    }
  }
  def ??[A]: A = ???
  def ??[A](message: String): A = throw new NotImplementedError(message)
}
