package mal

sealed trait MalType

object MalType {

  final case class Sym(name: String) extends MalType
  object Sym {
    object Def { def unapply(sym: Sym) = sym.name == "def!" }
    object Let { def unapply(sym: Sym) = sym.name == "let*" }
  }

  final case class Int(toInt: scala.Int) extends MalType {
    def +(other: Int): Int = ap(other, _ + _)
    def -(other: Int): Int = ap(other, _ - _)
    def *(other: Int): Int = ap(other, _ * _)
    def /(other: Int): Int = ap(other, _ / _)

    private def ap(other: Int, f: (scala.Int, scala.Int) => scala.Int): Int =
      Int(f(toInt, other.toInt))
  }

  final case class List(toList: scala.List[MalType]) extends MalType {
    def isEmpty: Boolean = toList.isEmpty
  }

  abstract class Func extends MalType {
    def apply(args: scala.List[MalType]): Either[Func.Error, MalType]
  }
  object Func {
    def apply(func: PartialFunction[scala.List[MalType], MalType]): Func =
      args => if (func.isDefinedAt(args)) Right(func(args)) else Left(Error.InvalidArgs(args))

    enum Error {
      case InvalidArgs(args: scala.List[MalType])
    }
  }


  // ===

  object ParseAsInt {
    def unapply(s: String): Option[MalType.Int] = s.toIntOption.map(MalType.Int(_))
  }
  object ParseAsSym {
    def unapply(s: String): Option[MalType.Sym] = Some(MalType.Sym(s))
  }
}
