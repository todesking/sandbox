package com.todesking.hoge

import scala.util.{Try, Success, Failure}

trait Transformer {
  def apply[A <: AnyRef](orig: Instance[A]): Try[Instance.Duplicate[A]] =
    try {
      Success(apply0(orig))
    } catch {
      case e: UnveilException => Failure(e)
    }

  def apply0[A <: AnyRef](orig: Instance[A]): Instance.Duplicate[A] =
    apply(orig).get
}
object Transformer {
  object fieldFusion extends Transformer {
    override def apply0[A <: AnyRef](orig: Instance[A]): Instance.Duplicate[A] = {
      val i = Transformer.lowerPrivateFields.apply0(orig)
      val targetFields =
        i.fields
          .filter { case (cr, f) => f.attribute.isFinal && !f.attribute.isStatic && (cr == i.thisRef || !f.attribute.isPrivate) }
          .filter { case (_, f) =>
            f.data match {
              case Data.Reference(typeRef, instance) =>
                instance.fields.forall { case (_, f) => f.attribute.isFinal && !f.attribute.isStatic }
              case _ =>
                false
            }
          }
      val newValues =
        i.fields.map {
          case ((cr, fr), f) =>
            f.data match {
              case Data.Reference(t, instance) =>
                val newInstance: Instance[_ <: AnyRef] = Transformer.lowerPrivateFields(instance).flatMap(Transformer.fieldFusion.apply) getOrElse instance
                ((cr -> fr) -> f.copy(data = Data.Reference(t, newInstance)))
              case _ =>
                ((cr -> fr) -> f)
            }
        }
      // TODO: replace the fields to fused version
      targetFields.keys.foldLeft(i.setFieldValues(newValues)) { case (i, (cr, fr)) => fieldFusion1(i, cr, fr) }
    }
  }

  // TODO: lowerPrivateMembers
  object lowerPrivateFields extends Transformer {
    override def apply0[A <: AnyRef](orig: Instance[A]): Instance.Duplicate[A] = {
      val dupInstance = orig.duplicate1
      // TODO: support invokespecial methods
      val entryMethods =
        dupInstance.virtualMethods
          .filterNot { case (mr, a) =>
            dupInstance.resolveVirtualMethod(mr) == ClassRef.Object || a.isFinal
          }.map { case (mr, a) =>
            mr -> dupInstance.dataflow(mr)
          }.filterNot { case (mr, df) =>
            // invokespecial need special handling
            df.body.bytecode.forall {
              case bc @ Bytecode.invokespecial(_, _)
              if df.mustInstance(bc.objectref, dupInstance) =>
                false
              case _ => true
            }
          }.filter { case (mr, df) =>
            df.usedFieldsOf(dupInstance).forall { case (cr, mr) =>
              val attr = dupInstance.fields(cr -> mr).attribute
              cr == dupInstance.thisRef || !attr.isPrivate || attr.isFinal
            }
          }
      val copyFields: Map[(ClassRef, FieldRef), (FieldRef, Field)] =
        entryMethods.flatMap {
          case (mr, df) =>
            df.usedFieldsOf(dupInstance).filter {
              case (cr, fr) => cr != dupInstance.thisRef && dupInstance.fields(cr -> fr).attribute.isPrivateFinal
            }
        }.map { case (cr, fr) =>
          val f = dupInstance.fields(cr -> fr)
          (cr -> fr) -> (fr.anotherUniqueName(cr.name, fr.name) -> f.copy(classRef = dupInstance.thisRef))
        }
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
  def fieldFusion1[A <: AnyRef](instance: Instance[A], classRef0: ClassRef, fieldRef: FieldRef): Instance.Duplicate[A] = {
    val dupInstance = instance.duplicate1
    // TODO: HOW IT WORKS????
    val classRef = if (classRef0 < dupInstance.thisRef.superClassRef) dupInstance.thisRef else classRef0
    instance.fields.get(classRef0, fieldRef).fold {
      throw new FieldAnalyzeException(classRef, fieldRef, s"Not found: ${classRef}.${fieldRef}")
    } { field =>
      if (!field.isFinal)
        throw new FieldTransformException(classRef, fieldRef, s"Not final")
      field.data match {
        case Data.Reference(t, fieldInstance) =>
          if (!fieldInstance.fields.values.forall(_.attribute.isFinal))
            throw new FieldTransformException(classRef, fieldRef, s"Can't fuse instance-stateful field")

          val usedMethods = dupInstance.usedMethodsOf(fieldInstance) ++ fieldInstance.usedMethodsOf(fieldInstance)
          val usedFields = dupInstance.usedFieldsOf(fieldInstance) ++ fieldInstance.usedFieldsOf(fieldInstance)

          val methodRenaming =
            usedMethods.map { case (cr, mr) => (cr -> mr) -> mr.anotherUniqueName(fieldRef.name, mr.name) }.toMap
          val fieldRenaming =
            usedFields.map { case (cr, fr) => (cr -> fr) -> fr.anotherUniqueName(fieldRef.name, fr.name) }.toMap

          // TODO[BUG]: use virtualMethods instead of methods: confused if multiple overriden method exists
          // TODO[BUG]: use usedVirtualMethods instead of usedMethods
          // TODO[BUG]: check no invokespecial(objectref = self)
          val targetMethods =
            dupInstance.methods.collect {
              case ((cr, mr), a)
              if cr < ClassRef.Object && !a.isAbstract && a.isVirtual && !a.isFinal && dupInstance.dataflow(cr, mr).usedMethodsOf(dupInstance).forall {
                case (cr, mr) => dupInstance.methods(cr -> mr).isVirtual
              } && dupInstance.dataflow(cr, mr).usedFieldsOf(dupInstance).forall {
                case (cr, fr) => cr == dupInstance.thisRef || dupInstance.fields(cr -> fr).attribute.isFinal
              } =>
              // TODO: check abstract/native
                (cr -> mr) -> dupInstance.methodBody(cr, mr)
            }

          val copyFields: Map[(ClassRef, FieldRef), (FieldRef, Field)] =
            targetMethods.flatMap {
              case ((cr, mr), b) =>
                b.dataflow(dupInstance).usedFieldsOf(dupInstance).filter {
                  case (cr, fr) => cr != dupInstance.thisRef && dupInstance.fields(cr -> fr).attribute.isPrivateFinal
                }
            }.map { case (cr, fr) =>
              val f = dupInstance.fields(cr -> fr)
              (cr -> fr) -> (fr.anotherUniqueName(cr.name, fr.name) -> f.copy(classRef = dupInstance.thisRef))
            }

            val (trueClassRef, trueFieldRef) =
              if(copyFields.contains(classRef -> fieldRef)) (dupInstance.thisRef -> copyFields(classRef -> fieldRef)._1)
              else (classRef -> fieldRef)

          def fusedMemberAccessRewriter(methodRef: MethodRef, df: DataFlow): PartialFunction[Bytecode, Seq[Bytecode]] = {
            import Bytecode._
            {
              case bc: InstanceFieldAccess if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
                Seq(bc.rewriteClassRef(dupInstance.thisRef).rewriteFieldRef(fieldRenaming(bc.classRef -> bc.fieldRef)))
              case bc @ invokespecial(cr, mr) if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
                Seq(bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(cr -> mr)))
              case bc @ invokevirtual(cr, mr) if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
                val definedCR = fieldInstance.resolveVirtualMethod(mr)
                Seq(bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(definedCR -> mr)))
              case bc @ areturn() if df.mustInstance(bc.objectref, fieldInstance) =>
                Seq(getfield(trueClassRef, trueFieldRef), areturn())
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

          val fieldResolvedMethods =
            targetMethods.map {
              case ((cr, mr), b) =>
                val df = b.dataflow(dupInstance)
                import Bytecode._
                (cr, mr) -> b.rewrite {
                  case bc: FieldAccess if copyFields.contains(bc.classRef -> bc.fieldRef) =>
                    val (fr, _) = copyFields(bc.classRef -> bc.fieldRef)
                    bc.rewriteClassRef(dupInstance.thisRef).rewriteFieldRef(fr)
                }
            }

          val additionalFields = fieldRenaming.map { case ((cr, fr), newRef) => newRef -> fieldInstance.fields(cr -> fr) } ++ copyFields.values
          val withNewFields = dupInstance.addFields(additionalFields)

          def thisMethods =
            fieldResolvedMethods.map {
              case ((cr, mr), body) =>
                val df = body.dataflow(withNewFields)
                import Bytecode._
                mr -> body.rewrite_*(({
                  case bc @ getfield(cr, fr) if cr == trueClassRef && fr == trueFieldRef && df.mustInstance(bc.objectref, withNewFields, mr, bc) =>
                    Seq(nop())
                }: PartialFunction[Bytecode, Seq[Bytecode]]).orElse(fusedMemberAccessRewriter(mr, df)))
            }

          def allMethods = fusedMethods ++ thisMethods
          assert(allMethods.size == fusedMethods.size + thisMethods.size)

          withNewFields.addMethods(allMethods)
        case other =>
          throw new FieldTransformException(classRef, fieldRef, s"Field can't fusable: ${other}")
      }
    }
  }
}
