package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

import java.lang.reflect.{ Method => JMethod, Field => JField, Modifier }

import com.todesking.scalapp.syntax._

object Main {
  def main(args: Array[String]): Unit = {
  }
}

object Graphviz {
  def drawAttr(attr: Seq[(Symbol, String)]) = s"""[${attr.map { case (k, v) => k.name + "=\"" + v + "\"" }.mkString(", ")}]"""
  def drawNode(id: String, attr: (Symbol, String)*) = s"""${id}${drawAttr(attr)}"""
  def drawEdge(from: String, to: String, attr: (Symbol, String)*) =
    s"""${from} -> ${to} ${drawAttr(attr)}"""
}

object Opt {
  def optimize[A <: AnyRef: ClassTag](orig: A): A = {
    val instance = Instance.of(orig)
    instance.value
  }
}

object Util {
  def tsort[A, B](in: Seq[A])(labelOf: A => B)(depsOf: A => Set[B]): Seq[A] =
    tsort0(in.map { i => (i, labelOf(i), depsOf(i)) }, Set.empty, Seq.empty)

  private[this] def tsort0[A, B](in: Seq[(A, B, Set[B])], deps: Set[B], sorted: Seq[A]): Seq[A] =
    if (in.isEmpty) {
      sorted
    } else {
      val (nodep, dep) = in.partition { case (a, b, bs) => bs.forall(deps.contains) }
      if (nodep.isEmpty) throw new IllegalArgumentException(s"Cyclic reference found: ${dep}")
      tsort0(dep, deps ++ nodep.map(_._2), sorted ++ nodep.map(_._1))
    }
}

final class InstructionLabel private () extends AbstractLabel
object InstructionLabel {
  def fresh(): InstructionLabel = new InstructionLabel
}

final class JumpTarget private extends AbstractLabel
object JumpTarget extends AbstractLabel.AssignerProvider[JumpTarget] {
  def fresh(): JumpTarget = new JumpTarget
}

sealed abstract class DataLabel private (val name: String) extends AbstractLabel
object DataLabel extends AbstractLabel.NamerProvider[DataLabel] {
  final class In(name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.In(${name})#${innerId}"
  }
  final class Out(name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.Out(${name})#${innerId}"
  }

  def in(name: String) = new In(name)
  def out(name: String) = new Out(name)
}

final class Effect private extends AbstractLabel
object Effect extends AbstractLabel.NamerProvider[Effect] {
  def fresh() = new Effect
}

class UniqueNamer() {
  private[this] var id = scala.util.Random.nextInt()
  private[this] def nextId(): Int = {
    id += 1
    id = math.abs(id)
    id
  }

  def apply(baseNames: String*): String = {
    val prefix = baseNames
      .map(_.replaceAll("""\$[0-9]+\$$""", ""))
      .map(_.replaceAll("[^A-Za-z0-9$]", ""))
      .mkString("__")
    prefix + "$" + nextId() + "$"
  }
}

trait Transformer[A <: AnyRef, B <: AnyRef] {
  def apply(orig: Instance[A]): Try[Instance[B]]
}
object Transformer {
  def fieldFusion[A <: AnyRef](instance: Instance[A], classRef: ClassRef, fieldRef: FieldRef): Instance.Duplicate[A] = {
    val dupInstance = instance.duplicate1
    instance.fields.get(classRef, fieldRef).fold {
      throw new IllegalArgumentException(s"Field not found: ${classRef}.${fieldRef}")
    } { field =>
      if (!field.isFinal)
        throw new IllegalArgumentException(s"Field ${classRef}.${fieldRef} is not final")
      field.data match {
        case Data.Reference(t, fieldInstance) =>
          if (!fieldInstance.fields.values.forall(_.attribute.isFinal))
            throw new IllegalArgumentException(s"Can't fuse instance-stateful field: ${classRef}.${fieldRef}")
          val usedMethods = (for {
            outer <- dupInstance.usedMethodsOf(fieldInstance)
            inner <- fieldInstance.usedMethodsOf(fieldInstance)
          } yield { outer ++ inner }) getOrElse { throw new IllegalArgumentException() }
          val usedFields = (for {
            outer <- dupInstance.usedFieldsOf(fieldInstance)
            inner <- fieldInstance.usedFieldsOf(fieldInstance)
          } yield { outer ++ inner }) getOrElse { throw new IllegalArgumentException() }

          // TODO: Make these REAL unique
          val methodRenaming =
            usedMethods.map { case (cr, mr) => (cr -> mr) -> mr.anotherUniqueName(s"${fieldRef.name}_${mr.name}") }.toMap
          val fieldRenaming =
            usedFields.map { case (cr, fr) => (cr -> fr) -> fr.anotherUniqueName(s"${fieldRef.name}_${fr.name}") }.toMap

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
                  case bc @ getfield(cr, mr) if df.onlyValue(bc.objectref).map(_.isInstance(i)) getOrElse false =>
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
          throw new IllegalArgumentException(s"Field can't fusionable: ${other}")
      }
    }
  }
}

