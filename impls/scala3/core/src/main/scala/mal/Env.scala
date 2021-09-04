package mal

import scala.collection.mutable
import scala.util.chaining.*
import scala.util.control.Breaks

final case class Env(
    outer: Option[Env],
    binds: Seq[MalType.Sym] = Seq.empty,
    exprs: Seq[MalType] = Seq.empty
) {
  private[this] val data: mutable.Map[MalType.Sym, MalType] = mutable.Map.empty

  if (binds.nonEmpty) {
    val bs = binds.toArray
    val es = exprs.toArray
    val b = new Breaks
    b.breakable {
      for (i <- 0 until binds.length) {
        if (bs(i).name == "&") { set(bs(i + 1), MalType.List(es.drop(i).toList)); b.break() }
        else set(bs(i), es(i))
      }
    }
  }

  def set(sym: MalType.Sym, value: MalType): Unit =
    data(sym) = value

  def find(sym: MalType.Sym): Option[MalType] =
    data.get(sym) orElse outer.flatMap(_.find(sym))

  def get(sym: MalType.Sym): MalType = {
//    println(toString)
    find(sym).getOrElse(sys.error(s"${sym.name} not found"))
  }

  override def toString: String = {
    val dataTable = {
      if (data.nonEmpty) {
        val maxKeyLength = data.keys.maxBy(_.name.length).name.length
        data
          .map { case (k, v) => s"    %-${maxKeyLength}s -> %s".format(k.name, v) }
          .toList
          .sorted
          .mkString("\n")
      } else {
        "    <empty>"
      }
    }
    s"""Env(
       |  outer = $outer
       |  binds = $binds
       |  exprs = $exprs
       |  data:
       |$dataTable
       |)""".stripMargin
  }
}
