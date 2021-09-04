package mal

import java.nio.file.{Files, Paths}
import scala.util.chaining._

object Core {
  lazy val ns: Seq[(MalType.Sym, MalType)] = Seq(
    MalType.Sym("+") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a + b },
    MalType.Sym("-") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a - b },
    MalType.Sym("*") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a * b },
    MalType.Sym("/") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a / b },
    MalType.Sym("<") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a < b },
    MalType.Sym(">") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a > b },
    MalType.Sym("<=") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a <= b },
    MalType.Sym(">=") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a >= b },
    //
    MalType.Sym("read-string") -> MalType.Func { case (s: MalType.String) :: _ =>
      Reader.readStr(s.rawString)
    },
    MalType.Sym("pr-str") -> MalType.Func { args =>
      MalType.String(args.map(Printer.prStr(_, printReadably = true)).mkString(" "))
    },
    MalType.Sym("str") -> MalType.Func { args =>
      MalType.String(args.toList.map(Printer.prStr(_, printReadably = false)).mkString)
    },
    //
    MalType.Sym("prn") -> MalType.Func { args =>
      println(args.map(Printer.prStr(_, printReadably = true)).mkString(" "))
      MalType.Nil()
    },
    MalType.Sym("println") -> MalType.Func { args =>
      println(args.map(Printer.prStr(_, printReadably = false)).mkString(" "))
      MalType.Nil()
    },
    //
    MalType.Sym("slurp") -> MalType.Func { case (s: MalType.String) :: _ =>
      MalType.String(new String(Files.readAllBytes(Paths.get(s.rawString))))
    },
    //
    MalType.Sym("list") -> MalType.Func(MalType.List(_)),
    MalType.Sym("list?") -> MalType.Func {
      case (_: MalType.List) :: _ => MalType.True()
      case _ :: _                 => MalType.False()
    },
    MalType.Sym("empty?") -> MalType.Func {
      case MalType.Seq(l) :: _ if l.isEmpty => MalType.True()
      case MalType.Nil() :: _               => MalType.True()
      case _ :: _                           => MalType.False()
    },
    MalType.Sym("count") -> MalType.Func {
      case MalType.Seq(l) :: _ => MalType.Int(l.length)
      case MalType.Nil() :: _  => MalType.Int(0)
    },
    //
    MalType.Sym("=") -> MalType.Func { case List(a, b) => MalType.Bool(eqMalType(a, b)) }
  )

  private def eqMalType(a: MalType, b: MalType): Boolean = {
    (a, b) match {
      case (MalType.Seq(xs), MalType.Seq(ys)) if xs.length == ys.length =>
        xs.zip(ys).forall(eqMalType)
      case _ =>
        a == b // 雑
    }
  }
}
