package com.todesking.scalanb.spark

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class Notebook extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroImpl.notebook.apply
}

