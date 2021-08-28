package mal
package step1

import scala.io.StdIn
import scala.util.Try
import scala.util.chaining.*
import scala.util.control.NonFatal

final class REPL {

  def read(src: String): MalType = Reader.readStr(src)

  def eval(input: MalType): MalType = input

  def print(tpe: MalType): String = Printer.prStr(tpe)

  def rep(): Unit =
    Iterator
      .continually(StdIn.readLine("user> "))
      .takeWhile(l => l != null && l != "" && l != ":q")
      .map { line =>
        if (line.nonEmpty) {
          try read(line).pipe(eval).pipe(print)
          catch { case NonFatal(e) => e.getMessage }
        } else {
          ""
        }
      }
      .foreach(println)
}
