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

trait Transformer[A <: AnyRef, B <: AnyRef] {
  def apply(orig: Instance[A]): Try[Instance[B]]
}
object Transformer {
  def fieldFusion[A <: AnyRef](instance: Instance[A], classRef: ClassRef, fieldRef: FieldRef): Instance.Duplicate[A] = {
    val dupInstance = instance.duplicate1
    instance.fields.get(classRef, fieldRef).fold {
      throw new IllegalArgumentException(s"Field not found: ${classRef.pretty}.${fieldRef.pretty}")
    } { field =>
      field.data match {
        case Data.Reference(t, fieldInstance) =>
          if (!fieldInstance.fields.values.forall(_.attribute.isFinal))
            throw new IllegalArgumentException(s"Can't fuse instance-stateful field: ${classRef.pretty}.${fieldRef.pretty}")
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
            usedMethods.map { case (cr, mr) => (cr -> mr) -> s"${fieldRef.name}__${mr.name}" }
          val fieldRenaming =
            usedFields.map { case (cr, fr) => (cr -> fr) -> s"${fieldRef.name}__${fr.name}" }

          val withNewFields =
            fieldRenaming.foldLeft(dupInstance) { case (i, ((cr, fr), newName)) =>
              i.addField(fr.renamed(newName), fieldInstance.fields(cr, fr))
            }

          def rewriteRefs(body: MethodBody): MethodBody = {
            def rewriteMethodRef(body: MethodBody): MethodBody =
              methodRenaming.foldLeft(body) { case (b, ((cr, mr), newName)) =>
                val df = b.dataflow(fieldInstance)
                import Bytecode._
                // TODO: I need Bytecode.Invoke.rewriteRef
                body.rewrite {
                  case bc @ invokevirtual(cref, mref)
                  if(cr == cref && mr == mref && df.dataValue(bc.receiver).isInstance(fieldInstance)) =>
                    invokevirtual(dupInstance.thisRef, mr.renamed(newName))
                  case bc @ invokespecial(cref, mref)
                  if(cr == cref && mr == mref && df.dataValue(bc.receiver).isInstance(fieldInstance)) =>
                    invokespecial(dupInstance.thisRef, mr.renamed(newName))
                }
              }
            def rewriteFieldRef(body: MethodBody): MethodBody =
              fieldRenaming.foldLeft(body) { case (b, ((cr, fr), newName)) =>
                val df = b.dataflow(fieldInstance)
                import Bytecode._
                body.rewrite {
                  // putfield is 
                  case bc @ getfield(cref, fref)
                  if(cr == cref && fr == fref && df.dataValue(bc.target).isInstance(fieldInstance)) =>
                    getfield(dupInstance.thisRef, fr.renamed(newName))
                }
              }
            rewriteFieldRef(rewriteMethodRef(body))
          }

          methodRenaming.foldLeft(withNewFields) { case (i, ((cr, mr), newName)) =>
            val body = rewriteRefs(fieldInstance.methodBody(cr, mr).get)
            i.addMethod(mr.renamed(newName), body)
          }
        case other =>
          throw new IllegalArgumentException(s"Field can't fusionable: ${other}")
      }
    }
  }
  /*
  def changeBaseClass[A <: AnyRef](baseClass: Class[A]): Transformer[A, A] = new Transformer[A, A] {
    override def apply(orig: Instance[A]): Try[Instance[A]] = {
      try {
        // TODO: handle protected/package method
        val newInstance = Instance.New(baseClass, ClassRef.newAnonymous(baseClass.getClassLoader, baseClass.getName))

        val baseRef = ClassRef.of(baseClass)

        val required = newInstance.methods.flatMap { m => requiredMethods(orig, newInstance, m) }

        import Dataflow.INode._
        import Bytecode._

        Success(Instance.Rewritten(
          newInstance,
          required.map { m =>
            val body = orig.methodBody(m) getOrElse { throw new TransformError(s"Can't acquire method body for ${m}") }
            m -> body.rewrite {
              case iv @ invokevirtual(classRef, method) if body.dataType(iv.receiver) == TypeRef.This && classRef < baseRef =>
                invokevirtual(newInstance.classRef, method)
            }
          }.toMap,
          useBaseClassRef = true
        ))
      } catch {
        case e: TransformError => Failure(e)
      }
    }

    private[this] def requiredMethods(
      orig: Instance[_ <: AnyRef],
      newInstance: Instance[_ <: AnyRef],
      m: LocalMethodRef,
      required: Set[LocalMethodRef] = Set.empty
    ): Set[LocalMethodRef] = {
      if(required.contains(m)) required
      else if(newInstance.sameMethodDefined(m, orig)) required
      else if(orig.isNativeMethod(m)) throw new TransformError(s"Method ${m.str} in ${orig.baseClass} is native")
      else if(orig.isAbstractMethod(m)) required
      else orig.methodBody(m).map { body =>
        import Dataflow.INode._
        body.dataflow.iNodes.collect {
          case InvokeVirtual(className, method, Data(TypeRef.This, _), retOption, args @ _*) =>
            method
          }.distinct.foldLeft(required) { (r, m) => (r + m) ++ requiredMethods(orig, newInstance, m, (r + m)) } + m
        // TODO: check other instance's protected/package private
        // TODO: check same-instance method call(virtual/special)
      }.getOrElse { throw new AssertionError() }
    }
  }
  */
}

