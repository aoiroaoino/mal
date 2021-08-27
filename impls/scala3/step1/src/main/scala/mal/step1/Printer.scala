package mal.step1

object Printer {

  def prStr(tpe: MalType): String = tpe match {
    case MalType.Sym(name)   => name
    case MalType.Int(i)      => i.toString
    case MalType.List(types) => types.map(prStr).mkString("(", " ", ")")
  }
}
