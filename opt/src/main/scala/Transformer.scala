package com.todesking.hoge

import scala.util.{Try, Success, Failure}

trait Transformer {
  def apply[A <: AnyRef](orig: Instance[A]): Try[Instance.Duplicate[A]]
}
object Transformer {
  object fieldFusion extends Transformer {
    override def apply[A <: AnyRef](orig: Instance[A]): Try[Instance.Duplicate[A]] = {
      try {
        val targetFields =
          orig.fields
            .filter { case (_, f) => f.attribute.isFinal && !f.attribute.isStatic }
            .filter { case (_, f) =>
              f.data match {
                case Data.Reference(typeRef, instance) =>
                  instance.fields.forall { case (_, f) => f.attribute.isFinal && !f.attribute.isStatic }
                case _ =>
                  false
              }
            }
        // TODO: replace the fields to fused version
        Success(targetFields.keys.foldLeft(orig.duplicate1) { case (i, (cr, fr)) => fieldFusion1(i, cr, fr) })
      } catch {
        case e: UnveilException => Failure(e)
      }
    }
  }

  def fieldFusion1[A <: AnyRef](instance: Instance[A], classRef0: ClassRef, fieldRef: FieldRef): Instance.Duplicate[A] = {
    // TODO: leak detection
    val dupInstance = instance.duplicate1
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

          def fusedMemberAccessRewriter(methodRef: MethodRef, df: DataFlow): PartialFunction[Bytecode, Bytecode] = {
            import Bytecode._
            {
              case bc: InstanceFieldAccess if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
                bc.rewriteClassRef(dupInstance.thisRef).rewriteFieldRef(fieldRenaming(bc.classRef -> bc.fieldRef))
              case bc @ invokespecial(cr, mr) if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
                bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(cr -> mr))
              case bc @ invokevirtual(cr, mr) if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
                val definedCR = fieldInstance.resolveVirtualMethod(mr)
                bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(definedCR -> mr))
            }
          }
          val fusedMethods =
            methodRenaming.map {
              case ((fCr, fMr), newMR) =>
                val b = fieldInstance.methodBody(fCr, fMr)
                val df = b.dataflow(fieldInstance)
                import Bytecode._
                newMR -> b.rewrite(fusedMemberAccessRewriter(fMr, df))
            }

          def thisMethods =
            dupInstance.thisMethods.map {
              case (mr, body) =>
                val df = body.dataflow(dupInstance)
                import Bytecode._
                mr -> body.rewrite(({
                  case bc @ getfield(cr, fr) if cr == classRef && fr == fieldRef && df.mustInstance(bc.objectref, dupInstance, mr, bc) =>
                    nop()
                }: PartialFunction[Bytecode, Bytecode]).orElse(fusedMemberAccessRewriter(mr, df)))
            }

          def allMethods = fusedMethods ++ thisMethods
          assert(allMethods.size == fusedMethods.size + thisMethods.size)

          val additionalFields = fieldRenaming.map { case ((cr, fr), newRef) => newRef -> fieldInstance.fields(cr -> fr) }

          dupInstance.addFields(additionalFields).addMethods(allMethods)
        case other =>
          throw new FieldTransformException(classRef, fieldRef, s"Field can't fusable: ${other}")
      }
    }
  }
}
