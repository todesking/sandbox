package com.todesking.hoge

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
