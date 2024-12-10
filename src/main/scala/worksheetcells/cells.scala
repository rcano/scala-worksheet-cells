package worksheetcells

import java.time.Instant
import java.security.MessageDigest

class Cell[T] private[Cell] (
    val name: String,
    val dependencies: Set[Cell[?]],
    private[Cell] val computeValue: () => T,
    reactionMechanism: Cell.ReactionMechanism,
) {
  def lastUpdate: Option[Instant] = Gwms.get(s"$name-last-update").map(_.asInstanceOf[Instant])
  def contentSha1: Option[String] = Gwms.getSha1(s"$name-value")

  private def getDependenciesContentHash(): String = {
    val shas = dependencies.toSeq.map(_.contentSha1.get)
    val d = MessageDigest.getInstance("SHA-1")
    shas.foreach(s => d.update(s.getBytes()))
    Aux.toHex(d.digest())
  }

  private def compute() = {
    val res = computeValue()
    Gwms.reeval(s"$name-value")(res)
    Gwms.reeval(s"$name-last-update")(Instant.now())

    reactionMechanism match {
      case Cell.ReactionMechanism.ContentHash =>
        // get the content hash of all our dependent cells
        // we just evaluated computeValue(), so our dependency cells must have a value evaluated and have a sha1
        Gwms.reeval(s"$name-deps-contents-hash")(getDependenciesContentHash())

      case _ =>
    }
    res
  }
  var lastComputed: T = Gwms.get(s"$name-value").map(_.asInstanceOf[T]).getOrElse(compute())
  def value: T = {
    reactionMechanism match {
      case Cell.ReactionMechanism.ContentHash =>
        val currentContentsHash = Gwms.get(s"$name-deps-contents-hash").asInstanceOf[Option[String]]
        currentContentsHash match {
          case Some(value) if value == getDependenciesContentHash() => // the hash is still the same, no need to reevaluate
          case _ => lastComputed = compute()
        }

      case Cell.ReactionMechanism.EvalTime =>
        val lu = lastUpdate
        if (lu.isEmpty || dependencies.flatMap(_.lastUpdate).exists(_.isAfter(lu.get))) lastComputed = compute()
    }

    lastComputed
  }

  /** Produce a new Cell based on the content of this cell. It'll be reevaluated if the hashes of this cell changes. */
  def map[U](f: T => U): Cell[U] = Tuple1(this).mapCells(t => f(t._1))

  /** Produce a new Cell based on the content of the mapped Cells. It'll be reevaluated if the evaluation time of this cell changes. */
  def mapTimeBased[U](f: T => U): Cell[U] = Tuple1(this).mapCellsTimeBased(t => f(t._1))
  override def toString = String.valueOf(value)

  def reeval(): this.type = {
    lastComputed = compute()
    this
  }
}

object Cell {
  enum ReactionMechanism {
    case EvalTime, ContentHash
  }

  def apply[T](name: String)(value: => T)(using scFile: sourcecode.FullName): Cell[T] = {
    new Cell[T](s"${scFile.value}:$name", Set.empty, () => value, ReactionMechanism.ContentHash)
  }

  def mapped[T <: Tuple: CellsTuple, R](
      cellsTup: T,
      reactionMechanism: ReactionMechanism,
  )(f: CellValues[T] => R): Cell[R] = {
    val cells = cellsTup.toArray.map(_.asInstanceOf[Cell[?]])
    new Cell[R](
      s"${cells.map(_.name).mkString("&")}:mapped",
      cells.toSet ++ cells.map(_.dependencies).reduce(_ ++ _),
      () => {
        val deps = Tuple.fromArray(cells.map(_.value))
        f(deps.asInstanceOf[CellValues[T]])
      },
      reactionMechanism
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

  /** Produce a new Cell based on the content of the mapped Cells. It'll be reevaluated if the hashes of the mapped cells changes. */
  def mapCells[R](f: CellValues[T] => R): Cell[R] = Cell.mapped(cellsTup, Cell.ReactionMechanism.ContentHash)(f)

  /** Produce a new Cell based on the content of the mapped Cells. It'll be reevaluated if the evaluation time of the mapped cells changes.
    */
  def mapCellsTimeBased[R](f: CellValues[T] => R): Cell[R] = Cell.mapped(cellsTup, Cell.ReactionMechanism.EvalTime)(f)
}
