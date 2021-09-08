package mal.step7

import mal.{Core, Env, MalType, Printer, Reader}

import java.nio.file.{Files, Paths}
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
      println("eval: " + (if (true) Printer.prStr(_ast) else _ast))
      _ast match {
        case MalType.List(Nil) =>
          return _ast

        case MalType.List(MalType.Sym.Def() :: (sym: MalType.Sym) :: v :: Nil) =>
          return eval(v, _env).tap(_env.set(sym, _))

        case MalType.List(MalType.Sym.Let() :: MalType.Seq(lets) :: t :: Nil) =>
          val letEnv = Env(outer = Some(_env))
          lets.grouped(2).foreach {
            case List(k: MalType.Sym, v) => eval(v, letEnv).pipe(letEnv.set(k, _))
            case _                       => sys.error(s"Invalid let*: $lets")
          }
          _env = letEnv
          _ast = t

        case MalType.List(MalType.Sym.Quote() :: t :: Nil) =>
          return t

        case MalType.List(MalType.Sym.Quasiquoteexpand() :: t :: Nil) =>
          return quasiquote(t)

        case MalType.List(MalType.Sym.Quasiquote() :: t :: Nil) =>
          _ast = quasiquote(t)

        case MalType.List(MalType.Sym.Do() :: ts) if ts.nonEmpty =>
          // `ts` is non-empty list. init and last will always succeed
          evalAst(MalType.List(ts.init), _env)
          _ast = ts.last

        case MalType.List(MalType.Sym.If() :: cond :: t :: f) =>
          eval(cond, _env) match {
            case MalType.False() | MalType.Nil() => _ast = if (f.isEmpty) MalType.Nil() else f.head
            case _                               => _ast = t
          }

        case MalType.List(MalType.Sym.Fn() :: MalType.Seq(ps) :: body :: Nil) =>
          return MalType.Func.UserDefined(
            ast = body,
            params = ps,
            env = _env,
            fn = MalType.Func { args =>
              val newEnv = Env(
                outer = Some(_env),
                binds = ps.map { p =>
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
                binds = f.params.map { p =>
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

  private def quasiquote(ast: MalType): MalType = ast match {
    case MalType.List(MalType.Sym.Unquote() :: t :: Nil) =>
      t
    case MalType.List(elt) =>
      elt.foldRight(MalType.List(List.empty) :: Nil) {
        case MalType.List(MalType.Sym.SpliceUnquote() :: t :: Nil) -> acc =>
          MalType.List(MalType.Sym("concat") :: t :: acc) :: Nil
        case t -> acc =>
          MalType.List(MalType.Sym("cons") :: quasiquote(t) :: acc) :: Nil
      }
        .pipe {
          case t :: Nil => t
          case ts => MalType.List(ts)
        }
    case ast: (MalType.HashMap | MalType.Sym) =>
      MalType.List(MalType.Sym.Quote() :: ast :: Nil)
    case ast =>
      ast
  }

  def evalAst(ast: MalType, env: Env): MalType = ast match {
    case t: MalType.Sym     => env.get(t)
    case t: MalType.List    => MalType.List(t.toList.map(eval(_, env)))
    case t: MalType.Vector  => MalType.Vector(t.toList.map(eval(_, env)))
    case t: MalType.HashMap => MalType.HashMap(t.toList.map { case (k, v) => (k, eval(v, env)) })
    case t                  => t
  }

  def print(tpe: MalType): String = Printer.prStr(tpe, printReadably = true)

  def start(args: List[String]): Unit = {
    val repEnv = Env(outer = None)
    Core.ns.foreach(repEnv.set)
    repEnv.set(MalType.Sym("eval"), MalType.Func {
      case t :: Nil => eval(t, repEnv)
      case _ => sys.error("too many args")
    })

    def rep(line: String): String = read(line).pipe(eval(_, repEnv)).pipe(print)
    rep("""(def! not (fn* (a) (if a false true)))""")
    rep("""(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))""")

    args match {
      case path :: args if isValidMalFile(path) =>
        repEnv.set(MalType.Sym("*ARGV*"), MalType.List(args.map(MalType.String(_))))
        if (isValidMalFile(path)) {
          rep(s"(load-file \"$path\")")
          sys.exit(0)
        }
      case _ =>
        repEnv.set(MalType.Sym("*ARGV*"), MalType.List(Nil))
    }

    Iterator
      .continually(StdIn.readLine("user> "))
      .takeWhile(l => l != null && l != ":q")
      .map { line =>
        if (line.nonEmpty) {
          try rep(line) + "\n"
          catch { case NonFatal(e) => e.getMessage + "\n" }
        } else {
          ""
        }
      }
      .foreach(println)
  }

  private def isValidMalFile(path: String): Boolean =
    try Files.isRegularFile(Paths.get(path))
    catch { case _ => false }
}
