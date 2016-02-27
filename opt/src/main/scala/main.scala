package com.todesking.hoge

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import java.lang.reflect.{ Method => JMethod , Field => JField, Modifier}

import com.todesking.scalapp.syntax._

object Main {
  def main(args: Array[String]): Unit = {
  }
}

case class FieldDescriptor(typeRef: TypeRef.Public) {
  def str = typeRef.str
}
object FieldDescriptor {
  def from(f: JField): FieldDescriptor =
    FieldDescriptor(TypeRef.from(f.getType))
  def parse(src: String, cl: ClassLoader): FieldDescriptor =
    Parsers.parseFieldDescriptor(src, cl)
}

object Graphviz {
  def drawAttr(attr: Seq[(Symbol, String)]) = s"""[${attr.map { case (k, v) => k.name + "=\"" + v + "\""}.mkString(", ")}]"""
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
      if(nodep.isEmpty) throw new IllegalArgumentException(s"Cyclic reference found: ${dep}")
      tsort0(dep, deps ++ nodep.map(_._2), sorted ++ nodep.map(_._1))
    }
}

case class Field(
  classRef: ClassRef,
  name: String,
  descriptor: FieldDescriptor,
  attribute: FieldAttribute,
  value: FieldValue
)
object Field {
  def from(f: JField, obj: AnyRef): Field =
    Field(
      ClassRef.of(f.getDeclaringClass),
      f.getName,
      FieldDescriptor.from(f),
      FieldAttribute.from(f),
      FieldValue.from(f, obj)
    )
}
sealed abstract class FieldValue {
  def value: Any
}
object FieldValue {
  def from(f: JField, obj: AnyRef): FieldValue = {
    val v = f.get(obj)
    TypeRef.from(f.getType) match {
      case _: TypeRef.Primitive => Primitive(v.asInstanceOf[AnyVal])
      case _: TypeRef.Reference if v == null => Null
      case _: TypeRef.Reference => Reference(Instance.of(v))
    }
  }

  case class Mutable(initialValue: Immutable) extends FieldValue {
    override def value = initialValue.value
  }
  sealed abstract class Immutable extends FieldValue
  case class Primitive(override val value: AnyVal) extends Immutable
  case class Reference(override val value: Instance[_ <: AnyRef]) extends Immutable
  case object Null extends Immutable {
    override val value = null
  }
}


sealed abstract class MethodAttribute {
  def |(that: MethodAttribute): MethodAttribute
  def enabled(flags: Int): Boolean
}
object MethodAttribute {
  def from(m: JMethod): MethodAttribute = {
    items.filter(_.enabled(m.getModifiers)).reduce[MethodAttribute](_ | _)
  }
  case class Multi(attrs: Set[MethodAttribute]) extends MethodAttribute {
    override def |(that: MethodAttribute): MethodAttribute = that match {
      case Multi(thats) => Multi(attrs ++ thats)
      case that => Multi(attrs + that)
    }
    override def enabled(flags: Int) = attrs.forall(_.enabled(flags))
  }

  sealed abstract class Single(flag: Int) extends MethodAttribute {
    override def |(that: MethodAttribute): MethodAttribute = that match {
      case Multi(thats) => Multi(thats + this)
      case that => Multi(Set(this, that))
    }
    override def enabled(flags: Int) = (flags & flag) == flag
  }
  case object Public extends Single(Modifier.PUBLIC)
  case object Private extends Single(Modifier.PRIVATE)
  case object Protected extends Single(Modifier.PROTECTED)
  case object Native extends Single(Modifier.NATIVE)
  case object Abstract extends Single(Modifier.ABSTRACT)
  case object Final extends Single(Modifier.FINAL)
  case object Synchronized extends Single(Modifier.SYNCHRONIZED)
  case object Strict extends Single(Modifier.STRICT)

  val items = Seq(Public, Private, Protected, Native, Final, Synchronized, Strict)
}
// TODO extract AttributeBase
sealed abstract class FieldAttribute {
  def |(that: FieldAttribute): FieldAttribute
  def enabled(flags: Int): Boolean
  def has(fa: FieldAttribute): Boolean
  def toModifiers: Int
}
object FieldAttribute {
  def from(m: JField): FieldAttribute = {
    items.filter(_.enabled(m.getModifiers)).reduce[FieldAttribute](_ | _)
  }
  case class Multi(attrs: Set[FieldAttribute.Single]) extends FieldAttribute {
    override def |(that: FieldAttribute): FieldAttribute = that match {
      case Multi(thats) => Multi(attrs ++ thats)
      case that: Single => Multi(attrs + that)
    }
    override def enabled(flags: Int) = attrs.forall(_.enabled(flags))
    override def has(fa: FieldAttribute): Boolean = fa match {
      case FieldAttribute.Multi(others) => this.attrs == others
      case a: FieldAttribute.Single => attrs.exists(_ == a)
    }
    def toModifiers = attrs.foldLeft[Int](0)(_ | _.flag)
  }

  sealed abstract class Single(val flag: Int) extends FieldAttribute {
    override def |(that: FieldAttribute): FieldAttribute = that match {
      case Multi(thats) => Multi(thats + this)
      case that: Single => Multi(Set(this, that))
    }
    override def enabled(flags: Int) = (flags & flag) == flag
    override def has(fa: FieldAttribute): Boolean = fa match {
      case FieldAttribute.Multi(attrs) => attrs.forall(_ == this)
      case s: FieldAttribute.Single => s == this
    }
    override def toModifiers = flag
  }
  case object Public extends Single(Modifier.PUBLIC)
  case object Private extends Single(Modifier.PRIVATE)
  case object Protected extends Single(Modifier.PROTECTED)
  case object Final extends Single(Modifier.FINAL)

  val items = Seq(Public, Private, Protected, Final)
}

case class Frame(locals: Map[Int, (DataLabel.Out, Data)], stack: List[(DataLabel.Out, Data)], effect: Effect) {
  def local(n: Int): (DataLabel.Out, Data) =
    locals(n)

  def stackTop: (DataLabel.Out, Data) = stack.head
}

case class Data(typeRef: TypeRef, value: Option[Any]) {
  def pretty: String = s"""${typeRef.pretty}${value.map { v => s" = ${v}" } getOrElse ""}"""
  def secondWordData: Data =
    if(!typeRef.isDoubleWord) throw new IllegalArgumentException()
    else Data(TypeRef.SecondWord, None)
}
object Data {
  val Undefined = Data(TypeRef.Undefined, None)
  def merge(d1: Data, d2: Data): Data = {
    if(d1 == d2) d1
    else {
      val t = TypeRef.common(d1.typeRef, d2.typeRef)
      val v =
        for {
          v1 <- d1.value
          v2 <- d2.value if same(v1, v2)
        } yield v1
      Data(t, v)
    }
  }
  private[this] def same(v1: Any, v2: Any): Boolean =
    (v1, v2) match {
      case (v1: AnyRef, v2: AnyRef) => v1 eq v2
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

