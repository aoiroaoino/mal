package mal

import scala.util.Try
import scala.util.control.NonFatal
import scala.util.chaining._

sealed trait MalType

object MalType {

  final case class Sym(name: scala.Predef.String) extends MalType
  object Sym {
    sealed abstract class SpecialForm(name: scala.Predef.String) {
      def apply(): Sym = MalType.Sym(name)
      def unapply(sym: Sym) = sym.name == name
    }
    object Def extends SpecialForm("def!")
    object Let extends SpecialForm("let*")
    object Do extends SpecialForm("do")
    object If extends SpecialForm("if")
    object Fn extends SpecialForm("fn*")
    object Quote extends SpecialForm("quote")
    object Quasiquote extends SpecialForm("quasiquote")
    object Quasiquoteexpand extends SpecialForm("quasiquoteexpand")
    object Unquote extends SpecialForm("unquote")
    object SpliceUnquote extends SpecialForm("splice-unquote")
  }

  final case class Keyword(name: scala.Predef.String) extends MalType {
    require(name.nonEmpty)
  }

  final class Atom private(var value: MalType) extends MalType {
    override def toString = s"Atom($value)"
  }
  object Atom {
    def apply(value: MalType): Atom = new Atom(value)
    def unapply(atom: Atom): Option[MalType] = Some(atom.value)
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

  final case class String(rawString: scala.Predef.String) extends MalType

  final case class Nil() extends MalType

  sealed trait Seq extends MalType {
    def toList: scala.List[MalType]

    def isVector: Boolean = !isList
    def isList: Boolean = !isVector
  }
  object Seq {
    def unapply(seq: Seq): Option[scala.List[MalType]] = Some(seq.toList)
  }

  final case class List(toList: scala.List[MalType]) extends MalType.Seq {
    override def productPrefix = "MalType.List"
    override def isList = true

    def isEmpty: Boolean = toList.isEmpty
    def length: scala.Int = toList.length
  }

  final case class Vector(toList: scala.List[MalType]) extends MalType.Seq {
    override def productPrefix = "MalType.Vector"
    override def isVector = true
  }

  final case class HashMap(toList: scala.List[(MalType, MalType)]) extends MalType {
    override def productPrefix = "MalType.HashMap"
  }
  object HashMap {
    def fromKeyValues(malTypes: scala.List[MalType]): HashMap = {
      HashMap(malTypes.iterator.grouped(2).map { case scala.Seq(k, v) => (k, v) }.toList)
    }
  }

  abstract class Func extends MalType {
    def apply(args: scala.List[MalType]): Either[Func.Error, MalType]
  }
  object Func {
    def apply(func: PartialFunction[scala.List[MalType], MalType]): Func =
      args =>
        if (func.isDefinedAt(args)) {
          Try(func(args)).toEither.left.map { case NonFatal(e) => Error.IOFailed(args, e) }
        } else {
          Left(Error.InvalidArgs(args))
        }

    def apply(func: scala.List[MalType] => MalType): Func =
      args => Right(func(args))

    final case class UserDefined(ast: MalType, params: scala.List[MalType], env: Env, fn: Func) extends MalType

    enum Error {
      case InvalidArgs(args: scala.List[MalType])
      case IOFailed(args: scala.List[MalType], cause: Throwable)
    }
  }
}
