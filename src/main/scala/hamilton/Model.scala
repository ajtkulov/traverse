package hamilton

import hamilton.Field.{isFinal, legalMoves}

import Bind._
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
  * Type constructor
  * @tparam F (* -> *) container
  */
trait Bindable[F[_]] {
  def flatMap1[A, B](v: Seq[A])(f: A => F[B]): F[B]
  def bind[B](a: B): F[B]
}

/**
  * Implementations for type constructor
  */
object Bind {
  implicit val iterable: Bindable[Iterator] = new Bindable[Iterator] {
    override def bind[B](a: B): Iterator[B] = Iterator.single(a)

    override def flatMap1[A, B](v: Seq[A])(f: (A) => Iterator[B]): Iterator[B] = v.iterator.flatMap(f)
  }

  implicit val optional: Bindable[Option] = new Bindable[Option] {
    override def flatMap1[A, B](v: Seq[A])(f: (A) => Option[B]): Option[B] = {
      v.foldLeft[Option[B]](None)((x, y) => x.orElse(f(y)))
    }

    override def bind[B](a: B): Option[B] = Some(a)
  }
}

/**
  * Traverse algorithm
  */
object Traverse {

  def traverseField[F[_]](cell: Cell, field: Field, state: State)(implicit b: Bindable[F]): F[State] = {
    if (isFinal(field, state)) {
      b.bind(state)
    } else {
      val moves = legalMoves(cell, field, state)

      val nextMoves: Seq[(Cell, State, Int)] = moves.map(item => {
        val newState = state.move(item)
        val possibleMovesAmount = legalMoves(item, field, newState)
        (item, newState, possibleMovesAmount.size)
      }).sortBy(_._3)

      b.flatMap1(nextMoves)(x => traverseField(x._1, field, x._2)(b))
    }
  }

  def traverseFieldIterator(cell: Cell, field: Field, state: State): Iterator[State] = {
    traverseField[Iterator](cell, field, state)
  }

  def traverseFieldOption(cell: Cell, field: Field, state: State): Option[State] = {
    traverseField[Option](cell, field, state)
  }

  def findPath(cell: Cell, field: Field): Option[Seq[Cell]] = {
    Traverse.traverseFieldIterator(cell, field, State.fromCell(cell)).take(1).toList.headOption.map(_.path)
  }

  def findPathOption(cell: Cell, field: Field): Option[Seq[Cell]] = {
    Traverse.traverseFieldOption(cell, field, State.fromCell(cell)).map(_.path)
  }
}