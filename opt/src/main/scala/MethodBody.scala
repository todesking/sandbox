package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod, Constructor => JConstructor }

import com.todesking.scalapp.syntax._

case class MethodBody(
    descriptor: MethodDescriptor,
    attribute: MethodAttribute,
    bytecode: Seq[Bytecode],
    jumpTargets: Map[JumpTarget, Bytecode.Label]
) {
  require(bytecode.nonEmpty)

  def isStatic: Boolean = attribute.isStatic

  // TODO: Exception handler

  def methodReferences: Set[(ClassRef, MethodRef)] =
    bytecode.collect { case bc: Bytecode.HasMethodRef => (bc.classRef -> bc.methodRef) }.toSet

  def fieldReferences: Set[(ClassRef, FieldRef)] =
    bytecode.collect { case bc: Bytecode.HasFieldRef => (bc.classRef -> bc.fieldRef) }.toSet

  def labelToBytecode(l: Bytecode.Label): Bytecode =
    bytecode.find(_.label == l).getOrElse { throw new IllegalArgumentException(s"Bytecode ${l} not found") }

  def rewrite(f: PartialFunction[Bytecode, Bytecode]): MethodBody = {
    val lifted = f.lift
    bytecode.foldLeft(this) {
      case (body, bc) =>
        val newBc = lifted(bc) getOrElse bc
        body.replaceBytecode(bc.label, newBc)
    }
  }

  def rewriteClassRef(from: ClassRef, to: ClassRef): MethodBody = {
    rewrite { case bc: Bytecode.HasClassRef if bc.classRef == from => bc.rewriteClassRef(to) }
  }

  def replaceBytecode(l: Bytecode.Label, newBc: Bytecode): MethodBody = {
    if (newBc.label == l) {
      this
    } else {
      val newBcs = bytecode.map { bc => if (bc.label == l) newBc else bc }
      val newJts = jumpTargets.map { case (jt, bcl) => if (bcl == l) (jt -> newBc.label) else (jt -> bcl) }
      MethodBody(descriptor, attribute, newBcs, newJts)
    }
  }

  def pretty: String = {
    val lName = Bytecode.Label.namer("L", "")
    s"""${
      bytecode.map { bc =>
        val l = f"L${bc.label.innerId}%-5s "
        l + (bc match {
          case j: Bytecode.Jump =>
            s"${j} # L${jumpTargets(j.target).innerId}"
          case b: Bytecode.Branch =>
            s"${b} # L${jumpTargets(b.target).innerId}"
          case b =>
            b.pretty
        })
      }.mkString("\n")
    }
"""
  }

  def dataflow(self: Instance[_ <: AnyRef]): DataFlow =
    new DataFlow(this, Data.Reference(self.thisRef.toTypeRef, self))
}

object MethodBody {
  def parse(m: JMethod): Option[MethodBody] =
    Javassist.decompile(m)

  def parse(m: JConstructor[_]): Option[MethodBody] =
    Javassist.decompile(m)
}

