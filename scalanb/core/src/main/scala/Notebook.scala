package com.todesking.scalanb

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class Notebook extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroImpl.notebook
}

