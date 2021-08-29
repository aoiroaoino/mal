package mal

object Printer {

  def prStr(tpe: MalType): String = tpe match {
    case MalType.Nil()       => "nil"
    case MalType.True()      => "true"
    case MalType.False()     => "false"
    case MalType.Sym(name)   => name
    case MalType.Int(i)      => i.toString
    case MalType.List(types) => types.map(prStr).mkString("(", " ", ")")
    case _: MalType.Func     => "#<function>"
  }
}
