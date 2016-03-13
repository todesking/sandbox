package com.todesking.hoge

import scala.util.Try

trait Transformer[A <: AnyRef, B <: AnyRef] {
  def apply(orig: Instance[A]): Try[Instance[B]]
}
object Transformer {
  def fieldFusion[A <: AnyRef](instance: Instance[A], classRef: ClassRef, fieldRef: FieldRef): Instance.Duplicate[A] = {
    val dupInstance = instance.duplicate1
    instance.fields.get(classRef, fieldRef).fold {
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

          val fusedMethods =
            methodRenaming.map { case ((fCr, fMr), newMR) =>
              val b = fieldInstance.methodBody(fCr, fMr).get
              val df = b.dataflow(fieldInstance)
              import Bytecode._
              (fCr, fMr) -> b.rewrite_** {
                case bc @ getfield(cr, mr) =>
                  df.ifOnlyInstance(bc.objectref, fieldInstance).fold {
                    throw new BytecodeTransformException(fCr, fMr, b, bc, "Ambigious rererence")
                  } { mustTheInstance =>
                    if(mustTheInstance)
                      Map(bc.label -> Seq(pop()))
                    else Map.empty
                  }
                case bc: InvokeInstanceMethod =>
                  df.ifOnlyInstance(bc.objectref, fieldInstance).fold {
                    throw new BytecodeTransformException(fCr, fMr, b, bc, "Ambigious rererence")
                  } { mustTheInstance =>
                    if(mustTheInstance) {
                      df.onlySourceBytecode(bc.objectref).fold {
                        println(df.sourceBytecodes(bc.objectref))
                        println(df.dataValue(bc.objectref))
                        throw new BytecodeTransformException(fCr, fMr, b, bc, s"Multiple/no receiver candidate: unsupported")
                      } {
                        case sbc @ aload(n) =>
                          Map(
                            sbc.label -> Seq(aload(0)),
                            bc.label -> Seq(bc.rewriteClassRef(dupInstance.thisRef).rewriteMethodRef(methodRenaming(bc.classRef, bc.methodRef)))
                          )
                        case _ =>
                          throw new BytecodeTransformException(fCr, fMr, b, bc, s"Unsupported objectref source")
                      }
                    } else {
                      Map.empty
                    }
                  }
              }
            }

          def rewriteRefs(instance: Instance[_ <: AnyRef], body: MethodBody): MethodBody = {
            def rewriteMethodRef(body: MethodBody): MethodBody =
              methodRenaming.foldLeft(body) {
                case (b, ((cr, mr), newMR)) =>
                  val df = b.dataflow(instance)
                  import Bytecode._
                  // TODO: I need Bytecode.Invoke.rewriteRef
                  b.rewrite {
                    case bc @ invokevirtual(cref, mref) if (cr == cref && mr == mref && df.onlyValue(bc.objectref).map(_.isInstance(fieldInstance)).getOrElse(false)) =>
                      invokevirtual(dupInstance.thisRef, newMR)
                    case bc @ invokespecial(cref, mref) if (cr == cref && mr == mref && df.dataValue(bc.objectref).isInstance(fieldInstance)) =>
                      invokespecial(dupInstance.thisRef, newMR)
                  }
              }
            def rewriteFieldRef(body: MethodBody): MethodBody =
              fieldRenaming.foldLeft(body) {
                case (b, ((cr, fr), newFR)) =>
                  val df = b.dataflow(instance)
                  import Bytecode._
                  body.rewrite {
                    case bc @ getfield(cref, fref) if (cr == cref && fr == fref && df.dataValue(bc.objectref).isInstance(fieldInstance)) =>
                      getfield(dupInstance.thisRef, newFR)
                  }
              }
            rewriteFieldRef(rewriteMethodRef(body))
          }

          val i1 =
            fieldRenaming.foldLeft(dupInstance) {
              case (i, ((cr, fr), newFR)) =>
                i.addField(newFR, fieldInstance.fields(cr, fr))
            }
          val i2 =
            i1.thisMethods.foldLeft(i1) {
              case (i, (mr, body)) =>
                import Bytecode._
                val df = body.dataflow(i)
                i.addMethod(mr, rewriteRefs(i, body.rewrite {
                  case bc @ getfield(cr, fr) if df.onlyValue(bc.objectref).map(_.isInstance(i)) getOrElse false =>
                    nop()
                  case bc @ invokevirtual(cr, mr) if df.onlyValue(bc.objectref).map(_.isInstance(fieldInstance)) getOrElse false =>
                    invokevirtual(i.thisRef, methodRenaming(cr -> mr))
                }))
            }
          val i3 =
            methodRenaming.foldLeft(i2) {
              case (i, ((cr, mr), newMR)) =>
                val body = rewriteRefs(fieldInstance, fieldInstance.methodBody(cr, mr).get)
                i.addMethod(newMR, body)
            }
          i3
        case other =>
          throw new FieldTransformException(classRef, fieldRef, s"Field can't fusionable: ${other}")
      }
    }
  }
}
