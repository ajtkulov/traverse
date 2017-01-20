package hamilton

import scala.collection.immutable.Seq

/**
  * Board from Task
  */
object TaskField extends SquareShiftBasedField {
  val size = 10

  override def shifts: Seq[Cell] = List(
    Cell(-3, 0),
    Cell(-2, 2),
    Cell(0, 3),
    Cell(2, 2),
    Cell(3, 0),
    Cell(2, -2),
    Cell(0, -3),
    Cell(-2, -2)
  )
}

/**
  * Chess board with knight
  */
object KnightChessField extends SquareShiftBasedField {
  val size = 8

  override def shifts: Seq[Cell] = List(
    Cell(1, 2),
    Cell(1, -2),
    Cell(-1, 2),
    Cell(-1, -2),
    Cell(2, -1),
    Cell(2, 1),
    Cell(-2, 1),
    Cell(-2, -1)
  )
}
