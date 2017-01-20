import hamilton._
import org.scalatest.FunSuite

import scala.collection.immutable.Seq

class Tests extends FunSuite {

  test("Test-1") {
    val path = Traverse.findPath(Cell(0, 0), TaskField)

    assert(path.isDefined)
    assert(isPathCorrect(path.get, TaskField))
  }

  test("Path from all possible start cell for TaskField") {
    val exceptCells = Set(Cell(3, 8), Cell(7, 4))
    for (row <- 0 to 9; column <- 0 to 9) {
      val cell = Cell(row, column)
      if (!exceptCells.contains(cell)) {
        val path = Traverse.findPath(cell, TaskField)

        assert(path.isDefined)
        assert(isPathCorrect(path.get, TaskField))
      }
    }
  }

  test("Cell(3, 8) case") {
    // solution for Cell(3, 8) runs in 45 sec. Pre-calculated. See Runners
    val path = List(Cell(3, 8), Cell(0, 8), Cell(0, 5), Cell(0, 2), Cell(2, 0), Cell(5, 0), Cell(8, 0), Cell(8, 3), Cell(8, 6), Cell(8, 9),
      Cell(5, 9), Cell(2, 9), Cell(0, 7), Cell(0, 4), Cell(0, 1), Cell(2, 3), Cell(2, 6), Cell(4, 8), Cell(1, 8), Cell(1, 5),
      Cell(1, 2), Cell(3, 0), Cell(0, 0), Cell(0, 3), Cell(2, 1), Cell(5, 1), Cell(8, 1), Cell(8, 4), Cell(8, 7), Cell(6, 9),
      Cell(9, 9), Cell(9, 6), Cell(7, 8), Cell(5, 6), Cell(5, 3), Cell(7, 1), Cell(4, 1), Cell(1, 1), Cell(3, 3), Cell(6, 3),
      Cell(9, 3), Cell(9, 0), Cell(6, 0), Cell(8, 2), Cell(8, 5), Cell(8, 8), Cell(6, 6), Cell(3, 6), Cell(0, 6), Cell(0, 9),
      Cell(3, 9), Cell(1, 7), Cell(1, 4), Cell(4, 4), Cell(4, 7), Cell(7, 7), Cell(7, 4), Cell(9, 2), Cell(6, 2), Cell(3, 2),
      Cell(1, 0), Cell(4, 0), Cell(2, 2), Cell(2, 5), Cell(2, 8), Cell(5, 8), Cell(5, 5), Cell(5, 2), Cell(7, 0), Cell(7, 3),
      Cell(9, 5), Cell(9, 8), Cell(6, 8), Cell(6, 5), Cell(3, 5), Cell(5, 7), Cell(2, 7), Cell(4, 5), Cell(7, 5), Cell(9, 7),
      Cell(7, 9), Cell(4, 9), Cell(1, 9), Cell(3, 7), Cell(6, 7), Cell(6, 4), Cell(3, 4), Cell(1, 6), Cell(1, 3), Cell(3, 1),
      Cell(6, 1), Cell(4, 3), Cell(4, 6), Cell(7, 6), Cell(5, 4), Cell(2, 4), Cell(4, 2), Cell(7, 2), Cell(9, 4), Cell(9, 1))

    assert(isPathCorrect(path, TaskField))
  }

  test("Cell(7, 4) case") {
    // solution for Cell(7, 4) runs in 206 sec. Pre-calculated. See Runners
    val path = List(Cell(7, 4), Cell(9, 2), Cell(7, 0), Cell(4, 0), Cell(1, 0), Cell(1, 3), Cell(1, 6), Cell(1, 9), Cell(4, 9), Cell(7, 9),
      Cell(9, 7), Cell(9, 4), Cell(9, 1), Cell(6, 1), Cell(3, 1), Cell(0, 1), Cell(0, 4), Cell(0, 7), Cell(2, 9), Cell(5, 9),
      Cell(8, 9), Cell(8, 6), Cell(8, 3), Cell(8, 0), Cell(5, 0), Cell(2, 0), Cell(0, 2), Cell(0, 5), Cell(0, 8), Cell(3, 8),
      Cell(6, 8), Cell(9, 8), Cell(9, 5), Cell(7, 7), Cell(9, 9), Cell(9, 6), Cell(7, 8), Cell(5, 6), Cell(2, 6), Cell(4, 8),
      Cell(1, 8), Cell(1, 5), Cell(3, 7), Cell(6, 7), Cell(8, 5), Cell(8, 8), Cell(5, 8), Cell(2, 8), Cell(4, 6), Cell(7, 6),
      Cell(7, 3), Cell(4, 3), Cell(2, 1), Cell(5, 1), Cell(8, 1), Cell(8, 4), Cell(8, 7), Cell(6, 9), Cell(6, 6), Cell(3, 6),
      Cell(0, 6), Cell(0, 9), Cell(3, 9), Cell(1, 7), Cell(4, 7), Cell(6, 5), Cell(6, 2), Cell(3, 2), Cell(3, 5), Cell(5, 7),
      Cell(2, 7), Cell(2, 4), Cell(5, 4), Cell(7, 2), Cell(9, 0), Cell(9, 3), Cell(7, 5), Cell(5, 3), Cell(7, 1), Cell(4, 1),
      Cell(2, 3), Cell(4, 5), Cell(6, 3), Cell(6, 0), Cell(4, 2), Cell(1, 2), Cell(3, 4), Cell(6, 4), Cell(8, 2), Cell(5, 2),
      Cell(5, 5), Cell(2, 5), Cell(2, 2), Cell(4, 4), Cell(1, 4), Cell(1, 1), Cell(3, 3), Cell(0, 3), Cell(0, 0), Cell(3, 0))
    assert(isPathCorrect(path, TaskField))
  }

  test("Path from all possible start cell for Knight") {
    for (row <- 0 to 7; column <- 0 to 7) {
      val path = Traverse.findPath(Cell(row, column), KnightChessField)

      assert(path.isDefined)
      assert(isPathCorrect(path.get, KnightChessField))
    }
  }

  test("Runners") {
    /*
      For almost all cells (excepts few) travers works in sec.
        There I use one optimisation (https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=vandermonde%20knight%27s%20tour), make a move to cell,
        which one has least possible moves. I have some chess experience ).

        Permutation for shifts-moves will break fast solution in different cells only.

     */
    //    println(Traverse.findPath(Cell(3, 8), TaskField))
    //    println(Traverse.findPath(Cell(7, 4), TaskField))
    println(Traverse.findPath(Cell(7, 4), KnightChessField))

  }

  def isPathCorrect(path: Seq[Cell], field: ShiftBasedField): Boolean = {
    val moves: Seq[Cell] = (0 until path.size - 1).map(x => minus(path(x), path(x + 1)))
    path.distinct.size == path.size && moves.forall(x => field.shifts.contains(x)) && path.size == field.fieldSize
  }

  def minus(fst: Cell, snd: Cell): Cell = {
    Cell(fst.row - snd.row, fst.column - snd.column)
  }
}
