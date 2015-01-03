package com.todesking.scalagp

import scala.util.parsing.combinator._

class Parser[A, C](repository: Repository[C]) extends RegexParsers {
  def expr: Parser[SExpr] = atom | list

  def atom = intNum | symbol
  def list = "(" ~> rep1(expr, expr) <~ ")" ^^ { list => SList(list) }

  def intNum = """[1-9][0-9]*""".r ^^ { s => Atom(s.toInt) }
  def symbol = """[a-z+*/^!?\-][a-z+*/^!?0-9\-]*""".r ^^ { s => Symbol(s) }

  def parse(klass: Class[A], source: String): Tree[A, C] =
    parseAll(expr, source) match {
      case Success(expr, _) =>
        toTree(klass, expr)
      case failure: NoSuccess =>
        throw new RuntimeException(s"Parse error: msg=${failure.msg}, source=${source}")
    }

  private[this] def toTree(klass: Class[A], expr: SExpr): Tree[A, C] = {
    val tree = toTree(expr)
    if(tree.definition.klass != klass)
      throw new IllegalArgumentException(s"parse result is ${tree} but required type is ${klass}")
    else
      tree.asInstanceOf[Tree[A, C]]
  }

  private[this] def toTree(expr: SExpr): Tree[_, C] = expr match {
    case SList(Seq(Symbol(sym), args@_*)) =>
      val definition = repository.definitionByName(sym)
      definition match {
        case Some(d: ConstLeafDefinition[A, C , Leaf[A, C]] @unchecked) =>
          if(args.size != 1)
            throw new IllegalArgumentException(s"Illegal argument size for ${d.name}: $expr")
          args match {
            case Seq(Atom(value)) =>
              // TODO: check value's class and definition's klass
              // (note: classOf[Int].isAssignableFrom(classOf[Integer]) is false)
              d.create(value.asInstanceOf[A])
            case _ =>
              throw new IllegalArgumentException(s"Illegal argument for ${d.name}: $expr")
          }
        case Some(d: LeafDefinition[A, C, Leaf[A, C]] @unchecked) =>
          if(args.size != d.arity)
            throw new IllegalArgumentException(s"Illegal argument size for ${d.name}: $expr")
          d.create()
        case Some(d: BranchDefinition[A, C, Branch[A, C, _]] @unchecked) =>
          if(args.size != d.arity)
            throw new IllegalArgumentException(s"Illegal argument size for ${d.name}: $expr")
          d.create(args.map { a => toTree(a) })
        case None =>
          throw new IllegalArgumentException(s"Invalid tree type: ${sym}")
      }
    case _ =>
      throw new IllegalArgumentException(s"Illegal expr: ${expr}")
  }
}

sealed abstract class SExpr
case class Symbol(name: String) extends SExpr {
  override def toString = name
}
case class Atom(value: Any) extends SExpr {
  override def toString = value.toString
}
case class SList(ars: Seq[SExpr]) extends SExpr {
  override def toString = s"(${ars.mkString(" ")})"
}
