package mal
package step2

import scala.io.StdIn
import scala.util.Try
import scala.util.chaining.*
import scala.util.control.NonFatal

final class REPL {
  type Env = Map[MalType.Sym, MalType]

  def read(src: String): MalType = Reader.readStr(src)

  def eval(ast: MalType, env: Env): MalType = ast match {
    case t @ MalType.List(_) if t.isEmpty => t
    case _: MalType.List =>
      evalAst(ast, env) match {
        case t: MalType.List =>
          t.toList match {
            case (x: MalType.Func) :: xs => x(xs).fold(e => sys.error(e.toString), identity)
            case _                       => sys.error(s"$t is not applicable")
          }
        case t => sys.error(s"Invalid state: ${t}")
      }
    case _ => evalAst(ast, env)
  }

  def evalAst(ast: MalType, env: Env): MalType = ast match {
    case t: MalType.Sym  => env.getOrElse(t, sys.error(s"Undefined: ${t.name}"))
    case t: MalType.List => MalType.List(t.toList.map(eval(_, env)))
    case t               => t
  }

  def print(tpe: MalType): String = Printer.prStr(tpe)

  def rep(): Unit = {
    val repEnv: Env = Map(
      MalType.Sym("+") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a + b },
      MalType.Sym("-") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a - b },
      MalType.Sym("*") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a * b },
      MalType.Sym("/") -> MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a / b }
    )
    Iterator
      .continually(StdIn.readLine("user> "))
      .takeWhile(l => l != null && l != ":q")
      .map { line =>
        if (line.nonEmpty) {
          try read(line).pipe(eval(_, repEnv)).pipe(print)
          catch { case NonFatal(e) => e.getMessage }
        } else {
          ""
        }
      }
      .foreach(println)
  }
}
