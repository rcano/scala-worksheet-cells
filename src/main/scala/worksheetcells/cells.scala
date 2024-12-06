package worksheetcells

import java.time.Instant

class Cell[T] private[Cell] (val name: String, val dependencies: Set[Cell[?]], private[Cell] val computeValue: () => T) {
  def lastUpdate: Option[Instant] = Gwms.get(s"$name-last-update").map(_.asInstanceOf[Instant])

  private def compute() = {
    val res = computeValue()
    Gwms.reeval(s"$name-value")(res)
    Gwms.reeval(s"$name-last-update")(Instant.now())
    res
  }
  var lastComputed: T = Gwms.get(s"$name-value").map(_.asInstanceOf[T]).getOrElse(compute())
  def value = {
    val lu = lastUpdate
    if (lu.isEmpty || dependencies.flatMap(_.lastUpdate).exists(_.isAfter(lu.get))) lastComputed = compute()

    lastComputed
  }

  def map[U](f: T => U): Cell[U] = Tuple1(this).mapCells(t => f(t._1))
  override def toString = String.valueOf(value)

  def reeval(): this.type = {
    lastComputed = compute()
    this
  }
}

object Cell {
  def apply[T](name: String)(value: => T)(using scFile: sourcecode.FullName): Cell[T] = {
    new Cell[T](s"${scFile.value}:$name", Set.empty, () => value)
  }

  def mapped[T <: Tuple: CellsTuple, R](
      cellsTup: T
  )(f: CellValues[T] => R): Cell[R] = {
    val cells = cellsTup.toArray.map(_.asInstanceOf[Cell[?]])
    new Cell[R](
      s"${cells.map(_.name).mkString("&")}:mapped",
      cells.toSet ++ cells.map(_.dependencies).reduce(_ ++ _),
      () => {
        val deps = Tuple.fromArray(cells.map(_.value))
        f(deps.asInstanceOf[CellValues[T]])
      }
    )
  }
}

type CellsTuple[T <: Tuple] = T match {
  case EmptyTuple => true =:= true
  case Cell[?] *: tail => CellsTuple[tail]
}
type CellValues[T <: Tuple] = T match {
  case EmptyTuple => EmptyTuple
  case Cell[h] *: tail => h *: CellValues[tail]
}

extension [T <: Tuple: CellsTuple](cellsTup: T) {
  def mapCells[R](f: CellValues[T] => R): Cell[R] = Cell.mapped(cellsTup)(f)
}