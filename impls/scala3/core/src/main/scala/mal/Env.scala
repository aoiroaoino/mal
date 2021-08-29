package mal

import scala.collection.mutable

final class Env(outer: Option[Env]) {
  private[this] val data: mutable.Map[MalType.Sym, MalType] = mutable.Map.empty

  def set(sym: MalType.Sym, value: MalType): Unit =
    data(sym) = value

  def find(sym: MalType.Sym): Option[MalType] =
    data.get(sym) orElse outer.flatMap(_.find(sym))

  def get(sym: MalType.Sym): MalType =
    find(sym).getOrElse(sys.error(s"${sym.name} not found"))
}
