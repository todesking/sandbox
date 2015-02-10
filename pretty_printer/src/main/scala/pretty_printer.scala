package com.todesking.pretty_printer

// from Christian et al. "Strictly Pretty"

object Example {
  import Doc._
   def binop(l: String, op: String, r: String) = Group(Nest(2, Group( (Text(l) ^| Text(op)) ^| Text(r))))

   val cond = binop("a", "==", "b")
   val expr1 = binop("a", "<<", "2")
   val expr2 = binop("a", "+", "b")
   def ifthen(c: Doc, e1: Doc, e2: Doc) =
     Group(
       Group(Nest(2, Text("if") ^| c))
       ^| Group(Nest(2, Text("then") ^| e1))
       ^| Group(Nest(2, Text("else") ^| e2))
     )
   val doc = ifthen(cond, expr1, expr2)
}

object PrettyPrinter {
  def pretty(w: Int, doc: Doc): String = {
    val sdoc = Doc.format(w, 0, Seq((0, Mode.Flat, Doc.Group(doc))))
    val str = SDoc.sdocToString(sdoc)
    str
  }
}

sealed abstract class Doc {
  def ^^(y: Doc) =
    Doc.Cons(this, y)
  def ^|(y: Doc) = (this, y) match {
    case (Doc.Nil, _) => y
    case (_, Doc.Nil) => this
    case (x, y) => x ^^ Doc.Break() ^^ y
  }
}
object Doc {
  case object Nil                       extends Doc
  case class Cons(car: Doc, cdr: Doc)   extends Doc
  case class Text(value: String)        extends Doc
  case class Nest(level: Int, doc: Doc) extends Doc
  case class Break(value: String = " ")       extends Doc
  case class Group(doc: Doc)            extends Doc

  def width(s: String): Int = s.size

  def fits(w: Int, ds: Seq[(Int, Mode, Doc)]): Boolean = ds match {
    case _ if w < 0 => false
    case Seq() => true
    case (i, m, Nil) :: z => fits(w, z)
    case (i, m, Cons(x, y)) :: z => fits(w, (i, m, x)::(i, m, y)::z)
    case (i, m, Nest(j, x)) :: z => fits(w, (i + j, m, x)::z)
    case (i, m, Text(s)) :: z => fits(w - width(s), z)
    case (i, Mode.Flat, Break(s)) :: z => fits(w - width(s), z)
    case (i, Mode.Break, Break(_)) :: z => true
    case (i, m, Group(x)) :: z => fits(w, (i, Mode.Flat, x)::z)
  }

  import SDoc._
  def format(w: Int, k: Int, ds: Seq[(Int, Mode, Doc)]): SDoc = ds match {
    case Seq() => SNil
    case (i, m, Nil) :: z => format(w, k, z)
    case (i, m, Cons(x, y)) :: z => format(w, k, (i, m, x)::(i, m, y)::z)
    case (i, m, Nest(j, x)) :: z => format(w, k, (i + j, m, x)::z)
    case (i, m, Text(s)) :: z => SText(s, format(w, k + width(s), z))
    case (i, Mode.Flat, Break(s)) :: z => SText(s, format(w, k + width(s), z))
    case (i, Mode.Break, Break(s)) :: z => SLine(i, format(w, i, z))
    case (i, m, Group(x)) :: z =>
      if(fits(w - k, (i, Mode.Flat, x)::z)) format(w, k, (i, Mode.Flat, x)::z)
      else format(w, k, (i, Mode.Break, x)::z)
  }
}

sealed abstract class Mode
object Mode {
  case object Flat  extends Mode
  case object Break extends Mode
}

/** "Simple" document for internal representation */
sealed abstract class SDoc
object SDoc {
  case object SNil                           extends SDoc
  case class SText(value: String, doc: SDoc) extends SDoc
  case class SLine(level: Int, doc: SDoc)    extends SDoc

  def sdocToString(sdoc: SDoc): String = sdoc match {
    case SNil                         => ""
    case SText(s, d)                  => s + sdocToString(d)
    case SLine(i, d)                  =>
      val prefix = " " * i
      "\n" + prefix + sdocToString(d)
  }
}
