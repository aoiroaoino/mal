package mal.step0

import scala.io.StdIn

final class REPL {

  def READ(s: String): String = s
  def EVAL(s: String): String = s
  def PRINT(s: String): String = s

  def rep(): Unit = {
    Iterator
      .continually(StdIn.readLine("user> "))
      .takeWhile(_ != "")
      .map(READ andThen EVAL andThen PRINT)
      .foreach(println)
  }
}
