package mal.step4

import scala.util.control.NonFatal

trait Command {
  private[this] lazy val repl = new REPL

  def main(args: Array[String]): Unit = {
    try repl.start()
    catch { case NonFatal(e) => e.printStackTrace(Console.err); sys.exit(1) }
  }
}
