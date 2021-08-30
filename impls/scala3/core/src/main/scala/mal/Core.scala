package mal

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
    MalType.Sym("prn") -> MalType.Func { case List(a) =>
      println(Printer.prStr(a))
      MalType.Nil()
    },
    //
    MalType.Sym("list") -> MalType.Func(MalType.List(_)),
    MalType.Sym("list?") -> MalType.Func {
      case (_: MalType.List) :: _ => MalType.True()
      case _ :: _                 => MalType.False()
    },
    MalType.Sym("empty?") -> MalType.Func {
      case (x: MalType.List) :: _ if x.isEmpty => MalType.True()
      case MalType.Nil() :: _                  => MalType.True()
      case _ :: _                              => MalType.False()
    },
    MalType.Sym("count") -> MalType.Func {
      case (x: MalType.List) :: _ => MalType.Int(x.length)
      case MalType.Nil() :: _     => MalType.Int(0)
    },
    //
    MalType.Sym("=") -> MalType.Func { case List(a, b) => MalType.Bool(eqMalType(a, b)) }
  )

  private def eqMalType(a: MalType, b: MalType): Boolean = {
    (a, b) match {
      case (MalType.List(xs), MalType.List(ys)) if xs.length == ys.length =>
        xs.zip(ys).forall(eqMalType)
      case _ =>
        a == b // 雑
    }
  }
}
