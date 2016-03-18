package com.todesking.hoge

import scala.language.existentials

import scala.collection.mutable

trait Transformer {
  def name: String
  def params: Map[String, String] = Map.empty

  def apply[A <: AnyRef](orig: Instance[A], el: Transformer.EventLogger): Transformer.Result[A] =
    try {
      el.enterTransformer(this, orig) { el => Transformer.Success(apply0(orig, el)) }
    } catch {
      case e: UnveilException =>
        Transformer.Failure(e)
    }

  protected[this] def apply0[A <: AnyRef](orig: Instance[A], el: Transformer.EventLogger): Instance.Duplicate[A]
}
object Transformer {
  sealed abstract class Result[A <: AnyRef] {
    def get: Instance.Duplicate[A]
    def getOrElse[B >: Instance.Duplicate[A]](a: => B): B
    def flatMap[B <: AnyRef](f: Instance.Duplicate[A] => Result[B]): Result[B]
  }
  case class Success[A <: AnyRef](instance: Instance.Duplicate[A]) extends Result[A] {
    override def get = instance
    override def getOrElse[B >: Instance.Duplicate[A]](a: => B) = instance
    override def flatMap[B <: AnyRef](f: Instance.Duplicate[A] => Result[B]) = f(instance)
  }
  case class Failure[A <: AnyRef](error: UnveilException) extends Result[A] {
    override def get = throw error
    override def getOrElse[B >: Instance.Duplicate[A]](a: => B) = a
    override def flatMap[B <: AnyRef](f: Instance.Duplicate[A] => Result[B]) = Failure[B](error)
  }

  class EventLogger {
    private[this] val eventBuffer = mutable.ArrayBuffer.empty[Event]

    private[this] def withSubEL[A](path: Path)(f: EventLogger => A): A = {
      val el = new EventLogger
      try {
        f(el)
      } catch {
        case e: Throwable =>
          el.fail(e)
          throw e
      } finally {
        eventBuffer += Event.Grouped(path, el.events)
      }
    }

    def clear(): Unit = eventBuffer.clear()

    def events: Seq[Event] = eventBuffer.toSeq

    def apply[A](f: EventLogger => A): A =
      try {
        f(this)
      } catch {
        case e: Throwable =>
          eventBuffer += Event.Fail(e)
          throw e
      }

    def enterField[A](cr: ClassRef, fr: FieldRef)(f: EventLogger => A): A = 
      withSubEL(Path.Field(cr, fr))(f)

    def enterTransformer[A](t: Transformer, i: Instance[_ <: AnyRef])(f: EventLogger => A): A = 
      withSubEL(Path.Transformer(t, i))(f)

    def section[A](title: String)(f: EventLogger => A): A =
      withSubEL(Path.Section(title))(f)
    
    def logCF(desc: String, cfs: Iterable[(ClassRef, FieldRef)]): Unit =
      eventBuffer += Event.CFs(desc, cfs.toSeq)

    def logCM(desc: String, cms: Iterable[(ClassRef, MethodRef)]): Unit =
      eventBuffer += Event.CMs(desc, cms.toSeq)

    def logMethods(desc: String, ms: Iterable[MethodRef]): Unit =
      eventBuffer += Event.Methods(desc, ms.toSeq)

    def fail(e: Throwable): Unit =
      eventBuffer += Event.Fail(e)

    def pretty(): String = pretty(0, events).split("\n").map { line =>
      val indent = (line.size - line.replaceAll("^ +", "").size) / 2
      f"$indent%1X$line"
    }.mkString("\n")

    private[this] def pretty(indent: Int, evs: Seq[Event]): String = {
      evs
        .map(prettyEvent)
        .flatMap(_.split("\n"))
        .map { s => "  " * indent + s }
        .mkString("\n")
    }

    private[this] def prettyEvent(event: Event): String = event match {
      case Event.Fail(e) =>
        s"FAIL: $e"
      case Event.Grouped(path, evs) =>
        prettyPath(path) + "\n" + pretty(1, evs)
      case Event.CFs(desc, cfs) =>
        s"$desc =" + (
          if(cfs.isEmpty) ""
          else cfs.map { case (cr, fr) => s"- $cr\n    .$fr" }.mkString("\n", "\n", "")
        )
      case Event.CMs(desc, cms) =>
        s"$desc =" + (
          if(cms.isEmpty) ""
          else cms.map { case (cr, mr) => s"- $cr\n    .$mr" }.mkString("\n", "\n", "")
        )
      case Event.Methods(desc, ms) =>
        s"$desc =" + (
          if(ms.isEmpty) ""
          else ms.map { case mr => s"- $mr" }.mkString("\n", "\n", "")
        )
    }

    private[this] def prettyPath(path: Path) = path match {
      case Path.Section(title) =>
        s"SECTION: $title"
      case Path.Field(cr, fr) =>
        s"""ENTERING FIELD: ${fr.name}
        |  class = $cr
        |  field = $fr""".stripMargin('|')
      case Path.Transformer(t, i) =>
        s"""APPLYING TRANSFORMER: ${t.name}""" + (
          if(t.params.isEmpty) ""
          else t.params.map { case (k, v) => s"  $k = $v" }.mkString("\n", "\n", "")
        ) + s"\n  instance = ${i.thisRef}"
    }
  }
  def newEventLogger(): EventLogger =
    new EventLogger

  sealed abstract class Path
  object Path {
    case class Field(classRef: ClassRef, fieldRef: FieldRef) extends Path
    case class Transformer(transformer: com.todesking.hoge.Transformer, instance: Instance[_ <: AnyRef]) extends Path
    case class Section(title: String) extends Path
  }
  sealed abstract class Event
  object Event {
    case class Fail(e: Throwable) extends Event
    case class Grouped(path: Path, events: Seq[Event]) extends Event
    case class CFs(desc: String, cfs: Seq[(ClassRef, FieldRef)]) extends Event
    case class CMs(desc: String, cfs: Seq[(ClassRef, MethodRef)]) extends Event
    case class Methods(desc: String, ms: Seq[MethodRef]) extends Event
  }

  object fieldFusion extends Transformer {
    override def name = "fieldFusion"
    override def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
      val i = Transformer.lowerPrivateFields.apply(orig, el).get
      val newValues =
        el.section("transform fields before fusion") { el =>
          i.fields.map {
            case ((cr, fr), f) =>
              el.enterField(cr, fr) { el =>
                f.data match {
                  case Data.Reference(t, instance) =>
                    val newInstance: Instance[_ <: AnyRef] =
                      // TODO: lowering not necessary
                      Transformer.lowerPrivateFields(instance, el).flatMap(Transformer.fieldFusion.apply(_, el)) getOrElse instance
                    ((cr -> fr) -> f.copy(data = Data.Reference(t, newInstance)))
                  case _ =>
                    ((cr -> fr) -> f)
                }
              }
          }
        }
      val newI = i.setFieldValues(newValues)
      val targetFields =
        newI.fields
          .filter { case ((cr, fr), f) => f.attribute.isFinal && !f.attribute.isStatic && (cr == newI.thisRef || !f.attribute.isPrivate) }
          .filter { case (_, f) =>
            f.data match {
              case Data.Reference(typeRef, instance) =>
                instance.fields.forall { case (_, f) => f.attribute.isFinal }
              case _ =>
                false
            }
          }
      el.logCF("target fields", targetFields.keys)
      targetFields.keys
        .foldLeft(newI) {
          case (i, (cr, fr)) =>
            fieldFusion1(cr, fr).apply(i, el) getOrElse i
        }
    }
  }

  // TODO: lowerPrivateMembers
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
      el.logMethods("entry methods", entryMethods.keys)
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
      el.logCF("lowered fields", copyFields.keys)
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
      el.logMethods("overriden methods", overridenMethods.keys)
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
      el.logCM("used methods of the field", usedMethods)

      val usedFields = dupInstance.usedFieldsOf(fieldInstance) ++ fieldInstance.usedFieldsOf(fieldInstance)
      el.logCF("used fields of the field", usedFields)

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
      el.logCM("rewrite target methods", targetMethods.keys)

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
    }
  }
}
