package mal.step5

import scala.util.control.NonFatal

trait Command {
  private[this] lazy val repl = new REPL

  def main(args: Array[String]): Unit = {
    try repl.rep()
    catch { case NonFatal(e) => e.printStackTrace(Console.err); sys.exit(1) }
  }
}
