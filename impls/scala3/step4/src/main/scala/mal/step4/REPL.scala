package mal
package step4

import mal.{Env, MalType, Printer, Reader}

import scala.collection.mutable.ListBuffer
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
        case MalType.Sym.Def() :: (sym: MalType.Sym) :: v :: Nil =>
          eval(v, env).tap(env.set(sym, _))
        case MalType.Sym.Let() :: (lets: MalType.List) :: t :: Nil =>
          val letEnv = new Env(outer = Some(env))
          lets.toList.grouped(2).foreach {
            case List(k: MalType.Sym, v) => eval(v, letEnv).pipe(letEnv.set(k, _))
            case _                       => sys.error(s"Invalid let*: $lets")
          }
          eval(t, letEnv)
        case MalType.Sym.Do() :: ts =>
          evalAst(MalType.List(ts), env) match {
            case t: MalType.List => t.toList.last
            case _               => sys.error("Unexpected error")
          }
        case MalType.Sym.If() :: cond :: t :: f =>
          eval(cond, env) match {
            case MalType.False() | MalType.Nil() =>
              f match {
                case Nil    => MalType.Nil()
                case x :: _ => eval(x, env)
              }
            case _ => eval(t, env)
          }
        case MalType.Sym.Fn() :: (ps: MalType.List) :: body :: Nil =>
          MalType.Func { case args =>
            val newEnv = new Env(
              outer = Some(env),
              binds = ps.toList.map { p =>
                require(p.isInstanceOf[MalType.Sym])
                p.asInstanceOf[MalType.Sym]
              },
              exprs = args
            )
            eval(body, newEnv)
          }
        case _ =>
          evalAst(ast, env) match {
            case t: MalType.List =>
              t.toList match {
                case (x: MalType.Func) :: xs =>
                  x(xs).fold(e => sys.error("Error: " + e.toString), identity)
                case _ =>
                  sys.error(s"Error: $t is not applicable")
              }
            case t => sys.error(s"Invalid state: ${t}")
          }
      }
    case _ => evalAst(ast, env)
  }

  def evalAst(ast: MalType, env: Env): MalType = ast match {
    case t: MalType.Sym  => env.get(t)
    case t: MalType.List => MalType.List(t.toList.map(eval(_, env)))
    case t               => t
  }

  def print(tpe: MalType): String = Printer.prStr(tpe)

  def rep(): Unit = {
    val repEnv = new Env(outer = None)
    Core.ns.foreach(repEnv.set)
    Iterator
      .continually(StdIn.readLine("user> "))
      .takeWhile(l => l != null && l != ":q")
      .map { line =>
        if (line.nonEmpty) {
          try read(line).pipe(eval(_, repEnv)).pipe(print) + "\n"
          catch { case NonFatal(e) => e.getMessage + "\n" }
        } else {
          ""
        }
      }
      .foreach(println)
  }
}
