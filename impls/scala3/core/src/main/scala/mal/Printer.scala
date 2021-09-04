package mal

import scala.util.chaining._

object Printer {

  def prStr(tpe: MalType, printReadably: Boolean = true): String = tpe match {
    case MalType.Nil()               => "nil"
    case MalType.True()              => "true"
    case MalType.False()             => "false"
    case MalType.Sym(name)           => name
    case MalType.Keyword(name)       => ":" + name
    case MalType.Atom(value)         => "(atom " + prStr(value, printReadably) + ")"
    case MalType.Int(i)              => i.toString
    case MalType.String(raw)         => prString(raw, printReadably)
    case MalType.List(types)         => types.map(prStr(_, printReadably)).mkString("(", " ", ")")
    case MalType.Vector(ts)          => ts.map(prStr(_, printReadably)).mkString("[", " ", "]")
    case MalType.HashMap(kvl)        => prKeyValueList(kvl, printReadably)
    case _: MalType.Func             => "#<function>"
    case _: MalType.Func.UserDefined => "#<function>"
  }

  private def prString(rawString: String, printReadably: Boolean): String =
    if (printReadably)
      rawString
        .replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\n", "\\n")
        .pipe("\"" + _ + "\"")
    else rawString

  private def prKeyValueList(l: List[(MalType, MalType)], printReadably: Boolean): String =
    l.flatMap { case (k, v) => List(k, v) }.map(prStr(_, printReadably)).mkString("{", " ", "}")
}
