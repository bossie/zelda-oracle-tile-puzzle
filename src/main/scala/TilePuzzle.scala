import scala.Vector

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

object TilePuzzle extends App {
  def count(grid: Grid, startPos: Position): Int = {
    def count0(grid: Grid, currentPos: Position): Int = {
      if (grid.solved) 1
      else grid.nextPositions(currentPos).foldLeft(0)((sum, nextPos) => sum + count0(grid occupy nextPos, nextPos))
    }

    count0(grid occupy startPos, startPos)
  }

  // http://faqsmedia.ign.com/faqs/image/zelda_oracle_of_ages_tile_3.gif
  val startPos = Position(6, 1)
  val occupieds: Iterable[Position] = List(
      Position(0, 2),
      Position(1, 2), Position(1, 7),
      Position(3, 1), Position(3, 4), Position(3, 8),
      Position(4, 3),
      Position(5, 7),
      Position(6, 2))

  val grid = occupieds.foldLeft(new Grid(7, 9))((grid, pos) => {
    grid occupy pos
  })

  println(grid)
  println(count(grid, startPos))
}
