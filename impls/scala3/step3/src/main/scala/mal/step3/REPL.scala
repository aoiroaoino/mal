package mal
package step3

import scala.io.StdIn
import scala.util.Try
import scala.util.chaining.*
import scala.util.control.NonFatal

final class REPL {

  def read(src: String): MalType = Reader.readStr(src)

  def eval(ast: MalType, env: Env): MalType = ast match {
    case t @ MalType.List(_) if t.isEmpty => t
    case t: MalType.List =>
      t.toList match {
        case MalType.Sym.Def() :: (sym: MalType.Sym) :: ts =>
          ts.pipe {
            case x :: Nil => x
            case xs       => MalType.List(xs)
          }.pipe(eval(_, env).tap(env.set(sym, _)))
        case MalType.Sym.Let() :: MalType.Seq(lets) :: ts =>
          val letEnv = new Env(outer = Some(env))
          lets.grouped(2).foreach {
            case List(k: MalType.Sym, v) => eval(v, letEnv).pipe(letEnv.set(k, _))
            case _                       => sys.error(s"Invalid let*: $lets")
          }
          ts.pipe {
            case x :: Nil => x
            case xs       => MalType.List(xs)
          }.pipe(eval(_, letEnv))
        case _ =>
          evalAst(ast, env) match {
            case t: MalType.List =>
              t.toList match {
                case (x: MalType.Func) :: xs => x(xs).fold(e => sys.error(e.toString), identity)
                case _                       => sys.error(s"$t is not applicable")
              }
            case t => sys.error(s"Invalid state: ${t}")
          }
      }
    case _ => evalAst(ast, env)
  }

  def evalAst(ast: MalType, env: Env): MalType = ast match {
    case t: MalType.Sym     => env.get(t)
    case t: MalType.List    => MalType.List(t.toList.map(eval(_, env)))
    case t: MalType.Vector  => MalType.Vector(t.toList.map(eval(_, env)))
    case t: MalType.HashMap => MalType.HashMap(t.toList.map { case (k, v) => (k, eval(v, env)) })
    case t                  => t
  }

  def print(tpe: MalType): String = Printer.prStr(tpe)

  def rep(): Unit = {
    val repEnv = (new Env(outer = None)).tap { env =>
      env.set(MalType.Sym("+"), MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a + b })
      env.set(MalType.Sym("-"), MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a - b })
      env.set(MalType.Sym("*"), MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a * b })
      env.set(MalType.Sym("/"), MalType.Func { case List(a: MalType.Int, b: MalType.Int) => a / b })
    }
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
