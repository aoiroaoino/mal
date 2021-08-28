package mal

enum MalType {
  case List(toList: scala.List[MalType])
  case Int(toInt: scala.Int)
  case Sym(name: String)
}

object MalType {
  object ParseAsInt {
    def unapply(s: String): Option[MalType.Int] = s.toIntOption.map(MalType.Int(_))
  }
  object ParseAsSym {
    def unapply(s: String): Option[MalType.Sym] = Some(MalType.Sym(s))
  }
}
