package mal

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
      case ReadAsNil(nil) => nil
      case ReadAsTrue(t)  => t
      case ReadAsFalse(f) => f
      case ReadAsInt(int) => int
      case ReadAsSym(sym) => sym
      case token          => sys.error(s"Invalid atom token: $token")
    }

  object ReadAsNil {
    def unapply(s: String): Option[MalType.Nil] =
      Option.when(s == "nil")(MalType.Nil())
  }
  object ReadAsTrue {
    def unapply(s: String): Option[MalType.True] =
      Option.when(s == "true")(MalType.True())
  }
  object ReadAsFalse {
    def unapply(s: String): Option[MalType.False] =
      Option.when(s == "false")(MalType.False())
  }
  object ReadAsInt {
    def unapply(s: String): Option[MalType.Int] =
      s.toIntOption.map(MalType.Int(_))
  }
  object ReadAsSym {
    def unapply(s: String): Option[MalType.Sym] =
      Some(MalType.Sym(s))
  }
}
