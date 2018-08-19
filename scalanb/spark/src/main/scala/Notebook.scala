package com.todesking.scalanb.spark

import com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.language.experimental.macros

class Notebook extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Notebook.macroImpl.apply
}
object Notebook {
  object macroImpl extends scalanb.Notebook.MacroImpl {
    override def makeRunMethod(c: Context)(stats: Seq[c.universe.Tree]): c.universe.Tree = {
      import c.universe._
      q"""
          def scalanb__run(scalanb__builder: _root_.com.todesking.scalanb.Builder, spark: _root_.org.apache.spark.sql.SparkSession): _root_.scala.Unit = {
            import _root_.com.todesking.scalanb.spark.Syntax._
            import spark.implicits._
            ..$stats
          }"""
    }

    override def makeMain(c: Context)(tpname: c.universe.TypeName, notebookName: String): c.universe.Tree = {
      import c.universe._
      q"""
      def main(args: Array[String]): Unit = {
        val target = new ${tpname}()
        _root_.com.todesking.scalanb.spark.Runner.runBatch(args, target, $notebookName)
      }
      """
    }
  }
}

