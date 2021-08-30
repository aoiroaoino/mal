package mal

sealed trait MalType

object MalType {

  final case class Sym(name: String) extends MalType
  object Sym {
    object Def { def unapply(sym: Sym) = sym.name == "def!" }
    object Let { def unapply(sym: Sym) = sym.name == "let*" }
    object Do { def unapply(sym: Sym) = sym.name == "do" }
    object If { def unapply(sym: Sym) = sym.name == "if" }
    object Fn { def unapply(sym: Sym) = sym.name == "fn*" }
  }

  sealed trait Bool extends MalType
  object Bool {
    def apply(boolean: Boolean): Bool = if (boolean) True() else False()
  }
  final case class True() extends Bool
  final case class False() extends Bool

  final case class Int(toInt: scala.Int) extends MalType {
    def +(other: Int): Int = ap(other, _ + _)
    def -(other: Int): Int = ap(other, _ - _)
    def *(other: Int): Int = ap(other, _ * _)
    def /(other: Int): Int = ap(other, _ / _)

    def <(other: Int): Bool = ap(other, _ < _)
    def <=(other: Int): Bool = ap(other, _ <= _)

    def >(other: Int): Bool = ap(other, _ > _)
    def >=(other: Int): Bool = ap(other, _ >= _)

    private def ap(other: Int, f: (scala.Int, scala.Int) => scala.Int): Int =
      Int(f(toInt, other.toInt))
    private def ap(other: Int, f: (scala.Int, scala.Int) => Boolean): Bool =
      Bool(f(toInt, other.toInt))
  }

  final case class Nil() extends MalType

  final case class List(toList: scala.List[MalType]) extends MalType {
    override def productPrefix = "MalType.List"

    def isEmpty: Boolean = toList.isEmpty
    def length: scala.Int = toList.length
  }

  abstract class Func extends MalType {
    def apply(args: scala.List[MalType]): Either[Func.Error, MalType]
  }
  object Func {
    def apply(func: PartialFunction[scala.List[MalType], MalType]): Func =
      args => if (func.isDefinedAt(args)) Right(func(args)) else Left(Error.InvalidArgs(args))

    def apply(func: scala.List[MalType] => MalType): Func =
      args => Right(func(args))

    enum Error {
      case InvalidArgs(args: scala.List[MalType])
    }
  }
}
