package mal

import scala.collection.mutable

final class Env(
    outer: Option[Env],
    binds: Seq[MalType.Sym] = Seq.empty,
    exprs: Seq[MalType] = Seq.empty
) {
  private[this] val data: mutable.Map[MalType.Sym, MalType] = mutable.Map.empty

  binds.zip(exprs).foreach { case (k, v) => set(k, v) }

  def set(sym: MalType.Sym, value: MalType): Unit =
    data(sym) = value

  def find(sym: MalType.Sym): Option[MalType] =
    data.get(sym) orElse outer.flatMap(_.find(sym))

  def get(sym: MalType.Sym): MalType =
    find(sym).getOrElse(sys.error(s"${sym.name} not found"))

  override def toString = data.toString
}
