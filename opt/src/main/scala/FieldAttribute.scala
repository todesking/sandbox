package com.todesking.hoge

import java.lang.reflect.{ Field => JField, Modifier }

sealed abstract class FieldAttribute extends Flags[FieldAttribute] {
  def isStatic: Boolean = has(FieldAttribute.Static)
  def isFinal: Boolean = has(FieldAttribute.Final)
}
object FieldAttribute extends FlagsCompanion[FieldAttribute] {
  def from(m: JField): FieldAttribute =
    items.filter(_.enabledIn(m.getModifiers)).reduce[FieldAttribute](_ | _)

  override def multi(items: Set[SingleFlag]): FieldAttribute =
    Multi(items)

  case class Multi(override val items: Set[SingleFlag]) extends FieldAttribute with MultiFlags

  sealed abstract class Single(val toInt: Int) extends FieldAttribute with SingleFlag

  case object Public extends Single(Modifier.PUBLIC)
  case object Private extends Single(Modifier.PRIVATE)
  case object Protected extends Single(Modifier.PROTECTED)
  case object Final extends Single(Modifier.FINAL)
  case object Static extends Single(Modifier.STATIC)

  val items = Seq(Public, Private, Protected, Final, Static)
}
