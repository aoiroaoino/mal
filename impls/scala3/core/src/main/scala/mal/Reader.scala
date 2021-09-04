package mal

import scala.util.chaining.*

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
      case "(" => readList(reader)
      case "[" => readVector(reader)
      case "{" => readHashMap(reader)
      case "@" => readAt(reader)
      case t   => readAtom(reader)
    }

  private def readList(reader: Reader): MalType =
    readValues(reader, start = "(", end = ")").pipe(MalType.List(_))

  private def readVector(reader: Reader): MalType =
    readValues(reader, start = "[", end = "]").pipe(MalType.Vector(_))

  private def readHashMap(reader: Reader): MalType =
    readValues(reader, start = "{", end = "}").pipe(MalType.HashMap.fromKeyValues)

  private def readAt(reader: Reader): MalType = {
    require(reader.next() == "@")
    MalType.List(MalType.Sym("deref") :: MalType.Sym(reader.next()) :: Nil)
  }

  private def readValues(
      reader: Reader,
      start: String,
      end: String
  ): List[MalType] = {
    require(reader.next() == start)
    val types = Iterator
      .continually(reader.peek())
      .takeWhile(_ != end)
      .map(_ => readFrom(reader))
      .toList
    require(reader.next() == end)
    types
  }

  private def readAtom(reader: Reader): MalType =
    reader.next() match {
      case s if s.startsWith(";") => readFrom(reader)
      case ReadAsNil(nil)         => nil
      case ReadAsTrue(t)          => t
      case ReadAsFalse(f)         => f
      case ReadAsInt(int)         => int
      case ReadAsString(s)        => s
      case ReadAsKeyword(k)       => k
      case ReadAsSym(sym)         => sym
      case token                  => sys.error(s"unbalanced token: $token")
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
  object ReadAsString {
    def unapply(s: String): Option[MalType.String] =
      Option.when(s.startsWith("\"") && s.endsWith("\"") && s.length > 1) {
        val i = (s.drop(1).dropRight(1)).iterator
        val b = new StringBuilder()
        while (i.hasNext) {
          val c = i.next()
          if (c != '\\') {
            b.append(c)
          } else if (i.hasNext) {
            i.next() match {
              case '\\' => b.append('\\').tap(println)
              case '"'  => b.append('\"')
              case 'n'  => b.append('\n')
              case _    => sys.error("unbalanced")
            }
          } else {
            sys.error("unbalanced")
          }
        }
        MalType.String(b.toString)
      }
  }
  object ReadAsKeyword {
    def unapply(s: String): Option[MalType.Keyword] =
      Option.when(s.startsWith(":")) {
        if (s.length < 2) sys.error("invalid keyword")
        else MalType.Keyword(s.drop(1))
      }
  }
  object ReadAsSym {
    def unapply(s: String): Option[MalType.Sym] =
      Option.when(!s.startsWith("\""))(MalType.Sym(s))
  }
}
