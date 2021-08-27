package mal.step1

import scala.util.chaining._

private final class Reader(tokens: Array[String]) {
  private var pos: Int = 0

  def next(): String = peek().tap(_ => pos += 1)
  def peek(): String = if (tokens.isDefinedAt(pos)) tokens(pos) else sys.error("EOF")
}

object Reader {

  def readStr(s: String): MalType = readFrom(new Reader(tokenize(s)))

  private val pattern =
    """[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)""".r

  private def tokenize(s: String): Array[String] =
    pattern
      .findAllMatchIn(s)
      .map(_.group(1))
      .collect { case t if t != null && t.nonEmpty => t }
      .toArray

  private def readFrom(reader: Reader): MalType =
    reader.peek() match {
      case "(" =>
        readList(reader)
      case t =>
        readAtom(reader)
    }

  private def readList(reader: Reader): MalType = {
    require(reader.next() == "(")
    val types = Iterator
      .continually(reader.peek())
      .takeWhile(_ != ")")
      .map(_ => readFrom(reader))
      .pipe(ts => MalType.List(ts.toList))
    require(reader.next() == ")")
    types
  }

  private def readAtom(reader: Reader): MalType =
    reader.next() match {
      case MalType.ParseAsInt(int) => int
      case MalType.ParseAsSym(sym) => sym
      case token                   => sys.error(s"Invalid atom token: $token")
    }
}
