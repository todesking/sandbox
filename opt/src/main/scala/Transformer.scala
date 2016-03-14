package com.todesking.hoge

import scala.util.Try

trait Transformer[A <: AnyRef, B <: AnyRef] {
  def apply(orig: Instance[A]): Try[Instance[B]]
}
object Transformer {
  def fieldFusion[A <: AnyRef](instance: Instance[A], classRef0: ClassRef, fieldRef: FieldRef): Instance.Duplicate[A] = {
    val dupInstance = instance.duplicate1
    val classRef = if(instance.thisRef == classRef0) dupInstance.thisRef else classRef0
    instance.fields.get(classRef0, fieldRef).fold {
      throw new FieldAnalyzeException(classRef, fieldRef, s"Not found: ${classRef}.${fieldRef}")
    } { field =>
      if (!field.isFinal)
        throw new FieldTransformException(classRef, fieldRef, s"Not final")
      field.data match {
        case Data.Reference(t, fieldInstance) =>
          if (!fieldInstance.fields.values.forall(_.attribute.isFinal))
            throw new FieldTransformException(classRef, fieldRef, s"Can't fuse instance-stateful field")
          val usedMethods = (for {
            outer <- dupInstance.usedMethodsOf(fieldInstance)
            inner <- fieldInstance.usedMethodsOf(fieldInstance)
          } yield { outer ++ inner }) getOrElse { throw new AnalyzeException("usedMethods") }
          val usedFields = (for {
            outer <- dupInstance.usedFieldsOf(fieldInstance)
            inner <- fieldInstance.usedFieldsOf(fieldInstance)
          } yield { outer ++ inner }) getOrElse { throw new AnalyzeException("usedFields") }

          val methodRenaming =
            usedMethods.map { case (cr, mr) => (cr -> mr) -> mr.anotherUniqueName(fieldRef.name, mr.name) }.toMap
          val fieldRenaming =
            usedFields.map { case (cr, fr) => (cr -> fr) -> fr.anotherUniqueName(fieldRef.name, fr.name) }.toMap

          def fusedMemberAccessRewriter(methodRef: MethodRef, df: DataFlow): PartialFunction[Bytecode, Bytecode] = {
            import Bytecode._
            {
                case bc :InstanceFieldAccess if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
                  bc.rewriteClassRef(dupInstance.thisRef).rewriteFieldRef(fieldRenaming(bc.classRef -> bc.fieldRef))
                case bc @ invokespecial(cr, mr) if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc)=>
                  bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(cr -> mr))
                case bc @ invokevirtual(cr, mr) if df.mustInstance(bc.objectref, fieldInstance, methodRef, bc) =>
                  val definedCR = fieldInstance.resolveVirtualMethod(mr)
                  bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(definedCR -> mr))
            }
          }
          val fusedMethods =
            methodRenaming.map { case ((fCr, fMr), newMR) =>
              val b = fieldInstance.methodBody(fCr, fMr).get
              val df = b.dataflow(fieldInstance)
              import Bytecode._
              newMR -> b.rewrite(fusedMemberAccessRewriter(fMr, df))
            }

          def thisMethods =
            dupInstance.thisMethods.map { case (mr, body) =>
              val df = body.dataflow(dupInstance)
              import Bytecode._
              mr -> body.rewrite(({
                case bc @ getfield(cr, fr)
                if cr == classRef && fr == fieldRef && df.mustInstance(bc.objectref, dupInstance, mr, bc) =>
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
