package hamilton

import Bind._

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val cell = Cell(1, 1)
    val iter: Iterator[State] = Traverse.traverseField[Iterator](cell, TaskField, State.fromCell(cell))
    println(iter.take(1).toList.head.path)
  }
}
