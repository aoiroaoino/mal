package mal
package step5

import mal.{Env, MalType, Printer, Reader}

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.Try
import scala.util.chaining.*
import scala.util.control.NonFatal

final class REPL {

  def read(src: String): MalType = Reader.readStr(src)

  def eval(ast: MalType, env: Env): MalType = {
    var _ast = ast
    var _env = env
    while (true) {
      // println("eval: " + _ast.toString)
      _ast match {
        case MalType.List(Nil) =>
          return _ast

        case MalType.List(MalType.Sym.Def() :: (sym: MalType.Sym) :: v :: Nil) =>
          return eval(v, _env).tap(_env.set(sym, _))

        case MalType.List(MalType.Sym.Let() :: (lets: MalType.List) :: t :: Nil) =>
          val letEnv = Env(outer = Some(_env))
          lets.toList.grouped(2).foreach {
            case List(k: MalType.Sym, v) => eval(v, letEnv).pipe(letEnv.set(k, _))
            case _                       => sys.error(s"Invalid let*: $lets")
          }
          _env = letEnv
          _ast = t

        case MalType.List(MalType.Sym.Do() :: ts) if ts.nonEmpty =>
          // `ts` is non-empty list. init and last will always succeed
          evalAst(MalType.List(ts.init), _env)
          _ast = ts.last

        case MalType.List(MalType.Sym.If() :: cond :: t :: f) =>
          eval(cond, _env) match {
            case MalType.False() | MalType.Nil() => _ast = if (f.isEmpty) MalType.Nil() else f.head
            case _                               => _ast = t
          }

        case MalType.List(MalType.Sym.Fn() :: (ps: MalType.List) :: body :: Nil) =>
          return MalType.Func.UserDefined(
            ast = body,
            params = ps,
            env = _env,
            fn = MalType.Func { args =>
              val newEnv = Env(
                outer = Some(_env),
                binds = ps.toList.map { p =>
                  require(p.isInstanceOf[MalType.Sym])
                  p.asInstanceOf[MalType.Sym]
                },
                exprs = args
              )
              eval(body, newEnv)
            }
          )

        case MalType.List(_) =>
          evalAst(_ast, _env) match {
            case MalType.List((f: MalType.Func) :: args) =>
              return f(args).fold(e => sys.error("Error: " + e.toString), identity)
            case MalType.List((f: MalType.Func.UserDefined) :: args) =>
              _ast = f.ast
              _env = Env(
                outer = Some(f.env),
                binds = f.params.toList.map { p =>
                  require(p.isInstanceOf[MalType.Sym])
                  p.asInstanceOf[MalType.Sym]
                },
                exprs = args
              )
            case t @ MalType.List(_) => sys.error(s"Error: $t is not applicable")
            case t => sys.error(s"Invalid state: ${t}")
          }

        case _ =>
          return evalAst(_ast, _env)
      }
    }
    sys.error(
      s"""Unexpected error...
         |  ast: $_ast
         |  env: $_env
         |""".stripMargin
    )
  }

  def evalAst(ast: MalType, env: Env): MalType = ast match {
    case t: MalType.Sym  => env.get(t)
    case t: MalType.List => MalType.List(t.toList.map(eval(_, env)))
    case t               => t
  }

  def print(tpe: MalType): String = Printer.prStr(tpe)

  def rep(): Unit = {
    val repEnv = Env(outer = None)
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
