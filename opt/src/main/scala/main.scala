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
    instance.fields.get(classRef, fieldRef).fold {
      throw new IllegalArgumentException(s"Field not found: ${classRef.pretty}.${fieldRef.pretty}")
    } { field =>
      field.value match {
        case FieldValue.Reference(instance) =>
          if (!instance.fields.values.forall(_.attribute.isStatic))
            throw new IllegalArgumentException(s"Can't fuse instance-stateful field: ${classRef.pretty}.${fieldRef.pretty}")
          val targetMethodBodies =
            instance.methods
              .filterNot { case ((cr, _), _) => cr == ClassRef.Object }
              .map {
                case ((cr, mr), a) =>
                  instance.methodBody(cr, mr) getOrElse {
                    throw new IllegalArgumentException(s"Cant rewrite method ${cr.pretty}.${mr.pretty}")
                  }
              }
          import Bytecode._
          // val methodsUsed = targetMethodBodies.flatMap { body =>
          //   val x = mutable.HashSet.empty[(ClassRef, MethodRef)]
          //   body.bytecode.foreach {
          //     case bc: InvokeInstanceMethod if body.dataValue(bc.target). =>
          //       x ++= bc.methodReference
          //   }
          //   x.toSet
          // }
          // val fieldUsed = targetMethodBodies.flatMap { body =>
          // }
          ???
        case other =>
          throw new IllegalArgumentException(s"Field can't fusionable: other")
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

