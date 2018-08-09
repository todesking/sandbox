package com.todesking.scalanb

import scala.reflect.macros.whitebox.Context

object MacroImpl {
  def notebook(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    annottees.map(_.tree) match {
      case Seq(q"class $tpname () { ..$stats }") =>
        val newStats =
          stats.flatMap {
            case st: DefTree =>
              val src = readContent(c)(st)
              Seq(
                q"scalanb__builder.code(${Literal(Constant(src))})",
                st)
            case expr =>
              Seq(
                q"scalanb__builder.code(${Literal(Constant(readContent(c)(expr)))})",
                q"scalanb__builder.expr($expr)")
          }
        val body = q"""
        def scalanb__run(scalanb__builder: _root_.com.todesking.scalanb.Builder): _root_.scala.Unit = {
          try {
            ..$newStats
          } catch {
            case e: _root_.scala.Throwable =>
              scalanb__builder.error(e)
          }
        }"""
        c.Expr[Any](q"class $tpname() { $body }")
    }
  }

  private[this] def readContent(c: Context)(t: c.Tree): String = {
    if (t.pos.source.content.isEmpty) {
      "<source unavailable>"
    } else {
      def dig(t: c.Tree): (Int, Int) = {
        val xs = (t.pos.start, t.pos.end) +: t.children.map(dig)
        (xs.map(_._1).min, xs.map(_._2).max)
      }
      val (start, end) = dig(t)
      t.pos.source.content.slice(start, end + 1).mkString("")
    }
  }
}
