package hamilton

import hamilton.Field.{isFinal, legalMoves}

import scala.collection.immutable.Seq

/**
  * Cell
  *
  * @param row    row
  * @param column column
  */
case class Cell(row: Int, column: Int) {
  def move(cell: Cell): Cell = {
    Cell(row + cell.row, column + cell.column)
  }
}

/**
  * Field
  */
trait Field {
  def neighborhood(cell: Cell): Seq[Cell]

  def fieldSize: Int
}

/**
  * Field, possible moves are defined by shift
  */
trait ShiftBasedField extends Field {
  def shifts: Seq[Cell]

  def isInside(cell: Cell): Boolean

  def neighborhood(cell: Cell): Seq[Cell] = {
    shifts.map(x => cell.move(x)).filter(isInside)
  }
}

/**
  * Square shape field
  */
trait SquareShiftBasedField extends ShiftBasedField {
  def size: Int

  lazy val fieldSize: Int = size * size

  override def isInside(cell: Cell): Boolean = {
    cell.row >= 0 && cell.row < size && cell.column >= 0 && cell.column < size
  }
}

/**
  * State for graph traversing
  * @param visited set of visited cells
  * @param path    linear path
  */
case class State(visited: Set[Cell], path: Seq[Cell]) {
  def isVisited(cell: Cell): Boolean = {
    visited.contains(cell)
  }

  def move(cell: Cell): State = {
    State(visited + cell, path :+ cell)
  }
}

/**
  * Companion object for State
  */
object State {
  def fromCell(cell: Cell): State = {
    State(Set(cell), List(cell))
  }
}

/**
  * Companion object for Field
  */
object Field {
  def legalMoves(cell: Cell, field: Field, state: State): Seq[Cell] = {
    field.neighborhood(cell).filter(x => !state.visited.contains(x))
  }

  def isFinal(field: Field, state: State): Boolean = {
    state.visited.size >= field.fieldSize
  }
}

/**
  * Traverse algorithm
  */
object Traverse {
  def traverseField(cell: Cell, field: Field, state: State): Iterator[State] = {
    if (isFinal(field, state)) {
      Iterator.single(state)
    } else {
      val moves = legalMoves(cell, field, state)

      val nextMoves: Iterator[(Cell, State, Int)] = moves.map(item => {
        val newState = state.move(item)
        val possibleMovesAmount = legalMoves(item, field, newState)
        (item, newState, possibleMovesAmount.size)
      }).sortBy(_._3).toIterator

      nextMoves.flatMap(x => traverseField(x._1, field, x._2))
    }
  }

  def canTraverse(cell: Cell, field: Field): Boolean = {
    Traverse.traverseField(cell, field, State.fromCell(cell)).take(1).toList.nonEmpty
  }

  def findPath(cell: Cell, field: Field): Option[Seq[Cell]] = {
    Traverse.traverseField(cell, field, State.fromCell(cell)).take(1).toList.headOption.map(_.path)
  }
}
