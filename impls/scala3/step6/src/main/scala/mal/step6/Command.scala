package mal.step6

import scala.util.control.NonFatal

trait Command {
  private[this] lazy val repl = new REPL

  def main(args: Array[String]): Unit = {
    try repl.start(args.toList)
    catch { case NonFatal(e) => e.printStackTrace(Console.err); sys.exit(1) }
  }
}
