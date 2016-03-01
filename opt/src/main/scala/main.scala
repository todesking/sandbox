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


trait Flags[Type <: Flags[Type]] {
  def |(that: Flags[Type]): Type
  def enabledIn(flags: Int): Boolean
  def has(flags: Flags[Type]): Boolean
  def toInt: Int
}

trait FlagsCompanion[Type <: Flags[Type]] {
  def multi(items: Set[SingleFlag]): Type

  trait MultiFlags extends Flags[Type] {
    def items: Set[SingleFlag]

    override def |(that: Flags[Type]): Type = that match {
      case that: MultiFlags => multi(items ++ that.items)
      case that: SingleFlag => multi(items + that)
    }

    override def enabledIn(flags: Int) = items.forall(_.enabledIn(flags))

    override def has(flags: Flags[Type]) = flags match {
      case that: MultiFlags => that.items.subsetOf(this.items)
      case that: SingleFlag => items.contains(that)
    }

    override def toString = s"${items.mkString(", ")}"

    override def toInt = items.foldLeft[Int](0)(_ | _.toInt)
  }
  trait SingleFlag extends Flags[Type] {
    override def |(that: Flags[Type]): Type = that match {
      case that: MultiFlags => multi(that.items + this)
      case that: SingleFlag => multi(Set(this, that))
    }

    override def enabledIn(flags: Int) =
      (flags & toInt) == toInt

    override def has(flags: Flags[Type]): Boolean = flags match {
      case that: MultiFlags => that.items.forall(has(_))
      case that: SingleFlag => this == that
    }
  }
}

sealed abstract class MethodAttribute extends Flags[MethodAttribute] {
  def isVirtual: Boolean =
    !this.has(MethodAttribute.Private) && !this.has(MethodAttribute.Static)
  def isStatic: Boolean =
    this.has(MethodAttribute.Static)
}
object MethodAttribute extends FlagsCompanion[MethodAttribute] {
  def from(m: JMethod): MethodAttribute =
    from(m.getModifiers)

  def from(flags: Int): MethodAttribute =
    items.filter(_.enabledIn(flags)).foldLeft[MethodAttribute](empty)(_ | _)

  val empty = Multi(Set.empty)

  override def multi(items: Set[SingleFlag]): MethodAttribute =
    Multi(items)

  case class Multi(override val items: Set[SingleFlag]) extends MethodAttribute with MultiFlags

  sealed abstract class Single(override val toInt: Int) extends MethodAttribute with SingleFlag

  case object Public extends Single(Modifier.PUBLIC)
  case object Private extends Single(Modifier.PRIVATE)
  case object Protected extends Single(Modifier.PROTECTED)
  case object Native extends Single(Modifier.NATIVE)
  case object Abstract extends Single(Modifier.ABSTRACT)
  case object Final extends Single(Modifier.FINAL)
  case object Synchronized extends Single(Modifier.SYNCHRONIZED)
  case object Strict extends Single(Modifier.STRICT)
  case object Static extends Single(Modifier.STATIC)

  val items = Seq(Public, Private, Protected, Native, Final, Synchronized, Strict, Static)
}
// TODO extract AttributeBase
sealed abstract class FieldAttribute extends Flags[FieldAttribute] {
  def toModifiers: Int
}
object FieldAttribute extends FlagsCompanion[FieldAttribute] {
  def from(m: JField): FieldAttribute =
    items.filter(_.enabledIn(m.getModifiers)).reduce[FieldAttribute](_ | _)

  override def multi(items: Set[SingleFlag]): FieldAttribute =
    Multi(items)

  case class Multi(override val items: Set[SingleFlag]) extends FieldAttribute with MultiFlags {
    def toModifiers = items.foldLeft[Int](0)(_ | _.toInt)
  }

  sealed abstract class Single(val toInt: Int) extends FieldAttribute with SingleFlag {
    override def toModifiers = toInt
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

