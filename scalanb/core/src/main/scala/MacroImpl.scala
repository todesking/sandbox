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
              throw new _root_.scala.RuntimeException("Error occured during processing notebook", e)
          }
        }"""
        c.Expr[Any](q"class $tpname() { $body }")
    }
  }

  private[this] def readContent(c: Context)(t: c.Tree): String = {
    if (t.pos == c.universe.NoPosition || t.pos.source.content.isEmpty) {
      "<source unavailable>"
    } else {
      t.pos.source.content.slice(t.pos.start, t.pos.end + 1).mkString("")
    }
  }
}
