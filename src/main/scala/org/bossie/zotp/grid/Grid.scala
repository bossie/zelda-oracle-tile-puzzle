package org.bossie.zotp.grid

case class Position(row: Int, column: Int) {
  override def toString = s"($row, $column)"
}

class Grid private (grid: Vector[Boolean], rows: Int) {
  def this(rows: Int, cols: Int) = this(Grid.empty(rows, cols), rows)

  private val cols = grid.size / rows

  val solved: Boolean = grid.forall(occupied => occupied)

  def occupy(pos: Position): Grid = {
    new Grid(grid.updated(pos.row * cols + pos.column, true), rows)
  }

  def nextPositions(currentPos: Position): Iterable[Position] = {
    def unavailable(nextRow: Int, nextCol: Int) =
      nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols || grid(nextRow * cols + nextCol)

    for {
      (dy, dx) <- Grid.neighbors
      nextRow = currentPos.row + dy
      nextCol = currentPos.column + dx
      available = !unavailable(nextRow, nextCol)
      if available
    } yield Position(nextRow, nextCol)
  }

  override def toString = {
    def asString(row: Vector[Boolean]) = row.map(occupied => if (occupied) 'x' else '.').mkString

    val rows = this.grid.grouped(cols)
    rows.map(asString).mkString("\n")
  }
}

object Grid {
  private def empty(rows: Int, cols: Int) = Vector.fill(rows * cols)(false)
  private val neighbors: Iterable[(Int, Int)] = List((-1, 0), (0, 1), (1, 0), (0, -1))
}
