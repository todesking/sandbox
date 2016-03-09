package com.todesking.hoge

import java.lang.reflect.{ Constructor, Method => JMethod }
import scala.util.{Try, Success, Failure }
import scala.collection.mutable

object Analyze {
  class SetterConstructor(
    val descriptor: MethodDescriptor,
    val superConstructor: Option[SetterConstructor],
    val constantAssigns0: Map[(ClassRef, FieldRef), Any],
    val argumentAssigns0: Map[(ClassRef, FieldRef), Int]
  ) {
    def methodRef: MethodRef = MethodRef.constructor(descriptor)
    def constantAssigns: Map[(ClassRef, FieldRef), Any] =
      superConstructor.map(_.constantAssigns).getOrElse(Map.empty) ++ constantAssigns0
    def argumentAssigns: Map[(ClassRef, FieldRef), Int] =
      superConstructor.map(_.argumentAssigns).getOrElse(Map.empty) ++ argumentAssigns0 // TODO: WRONG
    def toArguments(fields: Map[(ClassRef, FieldRef), Field]): Seq[Any] = {
      require(assignable(fields))
      if(descriptor.args.isEmpty) Seq.empty
      else ???
    }
    def sameArgumentValues: Seq[Set[(ClassRef, FieldRef)]] = ???
    def assignable(fields: Map[(ClassRef, FieldRef), Field]): Boolean = {
      fields forall { case ((cr, fr), f1) =>
        constantAssigns.get(cr -> fr).map { v2 =>
          isSameValue(fr.typeRef, f1.data.concreteValue, v2)
        } getOrElse {
          argumentAssigns.contains(cr -> fr) &&
            sameArgumentValues.forall { s =>
              !s.contains(cr -> fr) || sameArgumentValues.forall { s =>
                fields
                  .toSeq
                  .filter { case (k, f) => s.contains(k) }
                  .sliding(2)
                  .forall {
                    case Seq(((cr1, fr1), f1), ((cr2, fr2), f2)) =>
                      isSameValue(fr1.typeRef, f1.data.concreteValue, f2.data.concreteValue)
                    case Seq(_) => true
                  }
              }
            }
        }
      }
    }
    private[this] def isSameValue(t: TypeRef, v1: Any, v2: Any): Boolean =
      t match {
        case t: TypeRef.Primitive => v1 == v2
        case _ => ???
      }
    override def toString =
      s"""SetterConstructor(${descriptor}, ${constantAssigns}, ${argumentAssigns})"""

  }
  object SetterConstructor {
    def from(self: Instance[_ <: AnyRef], ctorClass: ClassRef, body: MethodBody): Try[SetterConstructor] = {
      val df = body.dataflow(self)
      import Bytecode._
      Try {
        var superConstructor: Option[SetterConstructor] = None
        val constAssigns = mutable.HashMap.empty[(ClassRef, FieldRef), Any]
        val argAssigns = mutable.HashMap.empty[(ClassRef, FieldRef), Int]
        body.bytecode.foreach {
              case bc: Shuffle =>
              case bc: Jump =>
              case bc: Return =>
              case bc: ConstX =>
              case bc @ invokespecial(classRef, methodRef)
              if df.onlyValue(bc.objectref).map(_.isInstance(self)).getOrElse(false) && methodRef.isInit =>
                // super ctor invocation
                if(superConstructor.nonEmpty)
                  throw new AnalyzeException(s"Another constructor called twice in ${ctorClass}.<init>${body.descriptor}")
                superConstructor =
                  SetterConstructor.from(self, classRef, self.methodBody(classRef, methodRef).get).map(Some(_)).get
              case bc @ putfield(classRef, fieldRef) if df.dataValue(bc.objectref).isInstance(self) =>
                df.dataValue(bc.value).value.map { v =>
                  // value from constant
                  constAssigns += (classRef -> fieldRef) -> v
                } getOrElse {
                  // value from argument
                  def argNum(label: DataLabel.Out): Option[Int] = {
                    val index = df.argLabels.indexOf(label)
                    if (index == -1) None else Some(index)
                  }
                  val l = df.dataBinding(bc.value)
                  argNum(l).fold {
                    throw new AnalyzeException(s"putfield non-argument/constant value is not acceptable")
                  } { i =>
                    argAssigns += (classRef -> fieldRef) -> i
                  }
                }
              case bc =>
                throw new AnalyzeException(s"Bytecode ${bc} is not acceptable in setter constructor")
            }
            new SetterConstructor(body.descriptor, superConstructor, constAssigns.toMap, argAssigns.toMap)
      }
    }
  }

  def setterConstructors( self: Instance[_ <: AnyRef], klass: Class[_]): Seq[SetterConstructor] = {
    val classRef = ClassRef.of(klass)
      klass
        .getDeclaredConstructors
        .filterNot { c => MethodAttribute.Private.enabledIn(c.getModifiers) }
        .map { c => MethodRef.from(c) }
        .map { mr => SetterConstructor.from(self, classRef, self.methodBody(classRef, mr).get) }
        .collect { case Success(sc) => sc }
  }


  def findSetterConstructor[A](
    self: Instance[_ <: AnyRef],
    klass: Class[A],
    fields: Map[(ClassRef, FieldRef), Field]
  ): Option[SetterConstructor] = {
    setterConstructors(self, klass)
        .filter { _.assignable(fields) }
        .headOption
  }
}

