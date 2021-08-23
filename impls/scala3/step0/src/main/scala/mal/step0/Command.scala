package mal.step0

trait Command {
  private[this] lazy val repl = new REPL

  def main(args: Array[String]): Unit = {
    repl.rep()
  }
}
