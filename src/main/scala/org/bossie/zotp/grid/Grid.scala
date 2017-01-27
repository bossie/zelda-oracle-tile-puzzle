package org.bossie.zotp.grid

case class Position(row: Int, column: Int) {
  require(row >= 0)
  require(column >= 0)

  override def toString = s"($row, $column)"
}

class Grid private (private val grid: Vector[Boolean], val rows: Int) {
  def this(rows: Int, cols: Int) = this(Grid.empty(rows, cols), rows)

  val columns = grid.size / rows

  val solved: Boolean = grid.forall(occupied => occupied)

  def occupy(pos: Position): Grid = {
    new Grid(grid.updated(pos.row * columns + pos.column, true), rows)
  }

  def nextPositions(currentPos: Position): Iterable[Position] = {
    def unavailable(nextRow: Int, nextCol: Int) =
      nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= columns || grid(nextRow * columns + nextCol)

    for {
      (dy, dx) <- Grid.neighbors
      nextRow = currentPos.row + dy
      nextCol = currentPos.column + dx
      available = !unavailable(nextRow, nextCol)
      if available
    } yield Position(nextRow, nextCol)
  }

  override def toString = {
    def asString(row: Vector[Boolean]) = row.map(occupied => if (occupied) '#' else '.').mkString

    val rows = this.grid.grouped(columns)
    rows.map(asString).mkString("\n")
  }
}

object Grid {
  private def empty(rows: Int, cols: Int) = Vector.fill(rows * cols)(false)
  private val neighbors: Iterable[(Int, Int)] = List((-1, 0), (0, 1), (1, 0), (0, -1))
}
