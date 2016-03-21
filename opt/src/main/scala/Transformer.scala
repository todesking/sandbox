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
          (cr -> fr) -> (fr.anotherUniqueName(fr.name) -> f)
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
  object fieldFusion extends  Transformer {
    override def name = s"fieldFusion"
    override def apply0[A <: AnyRef](instance: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
      val dupInstance = instance.duplicate1
      val fused = fuse(
        "",
        dupInstance,
        dupInstance.thisRef,
        dupInstance
          .rewritableVirtualMethods
          .keySet
          .filterNot { mr => dupInstance.resolveVirtualMethod(mr) == ClassRef.Object }
          .map { mr => dupInstance.resolveVirtualMethod(mr) -> mr } ++ (
            dupInstance
              .thisMethods
              .keySet
              .map { mr => dupInstance.thisRef -> mr }
          ),
          el
        )
      dupInstance
        .addMethods(fused.rewrittenMethods)
        .addMethods(fused.newMethods)
        .addFields(fused.newFields)
    }
    case class FuseResult(
      rewrittenMethods: Map[MethodRef, MethodBody],
      newMethods: Map[MethodRef, MethodBody],
      newFields: Map[FieldRef, Field]
    )
    // TODO: prevent inf loop
    private[this] def fuse(
      memberPrefix: String,
      self: Instance.Duplicate[_ <: AnyRef],
      thisRef: ClassRef,
      methods: Set[(ClassRef, MethodRef)],
      el: EventLogger
    ): FuseResult = {
      val dfs = methods.toSeq.map { case (cr, mr) => mr -> self.dataflow(cr, mr) }.toMap
      el.logCMethods("fuse target methods", methods)
      val usedFields: Set[(ClassRef, FieldRef)] =
        dfs.values
          .map(_.usedFieldsOf(self))
          .reduceLeftOption(_ ++ _).getOrElse(Set.empty)
          .filter { case (cr, fr) => self.fields(cr -> fr).attribute.isFinal }
          .filterNot { case (cr, fr) => cr != self.thisRef && self.fields(cr -> fr).attribute.isPrivate }
      el.logCFields("used fields from target methods", usedFields)

      val blankInstance = Instance.of(new AnyRef).duplicate1

      val (rewrittenMethods, newMethods, newFields) =
        usedFields.foldLeft(
          (dfs.mapValues(_.body), Map.empty[MethodRef, MethodBody], Map.empty[FieldRef, Field])
        ) { case ((methods, newMethods, newFields), (cr, fr)) =>
          el.enterField(cr, fr) { el =>
            self.fields(cr -> fr).data match {
               case Data.Reference(t, fieldInstance) if !fieldInstance.fields.forall(_._2.attribute.isFinal) =>
                 el.log("Pass: This field is instance-stateful")
                 (methods, newMethods, newFields)
               case Data.Reference(t, fieldInstance)  =>
                 val usedMethods =
                   fieldInstance.extendMethods(dfs.values.map(_.usedMethodsOf(fieldInstance)).reduceLeftOption(_ ++ _).getOrElse(Set.empty))
                 el.logCMethods("used methods", usedMethods)
                 val fused1 = el.section("fuse the field") { el =>
                   fuse(memberPrefix + fr.name + "__", fieldInstance.duplicate1, thisRef, usedMethods, el)
                 }
                 val methodRenaming = usedMethods.map { case k @ (cr, mr) => k -> mr.anotherUniqueName(memberPrefix + fr.name, mr.name) }.toMap
                 val fieldRenaming = fieldInstance.fields.keys.map { case k @ (cr, fr1) => k -> fr1.anotherUniqueName(memberPrefix + fr.name, fr1.name) }.toMap
                 val newFields1 = fieldRenaming.map { case((cr, fr), nfr) => nfr -> fieldInstance.fields(cr, fr) }
                 el.logCFields("rename target fields", fieldRenaming.keys)
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
                  val dummyInstance = self.addFields(newFields ++ newFields1)
                  val rewrittenMethods =
                    methods
                      .mapValues { b =>
                      val df = b.dataflow(dummyInstance)
                      import Bytecode._
                      val memberAccessOnly =
                        b.bytecode
                          .filter { bc => bc.inputs.exists { i => df.mustInstance(i, fieldInstance) } }
                          .forall {
                            case bc @ getfield(_, _) => true
                            case bc: InvokeInstanceMethod
                            if !bc.args.exists { a => df.mustInstance(a, fieldInstance) } => true
                            case bc => false
                          }
                      if(!memberAccessOnly) {
                        b
                      } else {
                        b.rewrite_* {
                          case bc @ getfield(cr1, fr1)
                          if df.mustThis(bc.objectref) && cr1 == cr && fr1 == fr =>
                            Seq(nop())
                          case bc: InvokeInstanceMethod
                          if df.mustInstance(bc.objectref, fieldInstance) =>
                            Seq(
                              methodRenaming.get(bc.classRef, bc.methodRef).fold(bc) { mr =>
                                bc.rewriteMethodRef(thisRef, mr)
                              }
                            )
                          case bc: InstanceFieldAccess
                          if df.mustInstance(bc.objectref, fieldInstance) =>
                            Seq(
                              fieldRenaming.get(bc.classRef, bc.fieldRef).fold(bc) { case fr =>
                                bc.rewriteFieldRef(thisRef, fr)
                              }
                            )
                        }
                      }
                    }
                  (rewrittenMethods, newMethods ++ newMethods1, newFields ++ newFields1)
               case _ =>
                 el.log("Pass")
                 (methods, newMethods, newFields)
            }
          }
        }
      el.logMethods("rewritten methods", rewrittenMethods.keys)
      el.logMethods("new methods", newMethods.keys)
      el.logFields("new fields", newFields.keys)
      FuseResult(rewrittenMethods, newMethods, newFields)
    }
  }
  def ??[A]: A = ???
  def ??[A](message: String): A = throw new NotImplementedError(message)
}
