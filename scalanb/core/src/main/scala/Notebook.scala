package com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.language.experimental.macros

class Notebook extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Notebook.macroImpl.apply
}

object Notebook {
  trait MacroImpl {
    def processStats(c: Context)(stats: Seq[c.universe.Tree]): Seq[c.universe.Tree] = {
      import c.universe._
      stats.flatMap {
        case st if st.isDef || st.isType || !st.isTerm => // TODO: I don't know how to detect not-a-value trees
          val src = readContent(c)(st)
          Seq(
            q"scalanb__builder.code(${Literal(Constant(src))})",
            st)
        case expr =>
          Seq(
            q"scalanb__builder.code(${Literal(Constant(readContent(c)(expr)))})",
            q"scalanb__builder.expr($expr)")
      }
    }

    def makeRunMethod(c: Context)(stats: Seq[c.universe.Tree]): c.universe.Tree = {
      import c.universe._
      q"""
          def scalanb__run(scalanb__builder: _root_.com.todesking.scalanb.Builder): _root_.scala.Unit = {
              ..$stats
          }"""
    }

    def makeMain(c: Context)(tpname: c.universe.TypeName, notebookName: String): c.universe.Tree = {
      import c.universe._
      q"""
      def main(args: Array[String]): Unit = {
        val target = new ${tpname}()
        _root_.com.todesking.scalanb.Runner.runBatch(args, target, $notebookName)
      }
      """
    }

    def apply(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      annottees.map(_.tree) match {
        case Seq(q"class $tpname () { ..$stats }") =>
          val notebookName = tpname.toString
          val runMethod = makeRunMethod(c)(processStats(c)(stats))
          val mainMethod = makeMain(c)(tpname, notebookName)
          c.Expr[Any](q"""
            class $tpname() {
              $runMethod
            }
            object ${tpname.toTermName} {
              $mainMethod
            }
          """)
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
  object macroImpl extends MacroImpl
}
