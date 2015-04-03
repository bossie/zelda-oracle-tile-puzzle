import scala.Vector

case class Position(row: Int, column: Int)

class Grid private (grid: Vector[Vector[Boolean]], endPos: Position, occupied: Int) {
  def this(rows: Int, cols: Int, endPos: Position) = this(Grid.empty(rows, cols), endPos, 0)
  
  private val rows = grid.length
  private val cols = grid(0).length
  val solved: Boolean = occupied == rows * cols

  def occupy(pos: Position): Grid = {
    val updatedRow = grid(pos.row).updated(pos.column, true)
    new Grid(grid.updated(pos.row, updatedRow), endPos, occupied + 1)
  }

  def nextPositions(currentPos: Position): List[Position] = {
    def unavailable(nextPos: Position) = {
      nextPos.row < 0 || nextPos.row >= rows || nextPos.column < 0 || nextPos.column >= cols ||
         (nextPos == endPos && occupied < (rows * cols - 1)) || grid(nextPos.row)(nextPos.column)
    }

    List((-1, 0), (0, 1), (1, 0), (0, -1)) flatMap {
      case (dy, dx) => {
        val nextPos = Position(currentPos.row + dy, currentPos.column + dx)

        if (unavailable(nextPos)) None
        else Some(nextPos)
      }
    }
  }
  
  override def toString = grid.zipWithIndex map {
    case (row, j) => row.zipWithIndex map {
      case (occupied, i) => {
        if (j == endPos.row && i == endPos.column) 'X'
        else if (occupied) '#'
        else '.'
      }
    } mkString
  } mkString ("\n")
}

object Grid {
  private def empty(rows: Int, cols: Int): Vector[Vector[Boolean]] = {
    def emptyRow: Vector[Boolean] = Vector(((0 until cols) map { col => false }):_*)
    Vector(((0 until rows) map { row => emptyRow }):_*)
  }
}

object TilePuzzle extends App {
  def count(grid: Grid, currentPos: Position): Int = {
    if (grid.solved) 1
    else grid.nextPositions(currentPos).par.foldLeft(0)((sum, nextPos) => sum + count(grid occupy nextPos, nextPos))
  }
  
  val startPos = Position(1, 0)
  val endPos = Position(0, 2)
  val occupieds = List()
  
  val grid = occupieds.foldLeft(new Grid(2, 3, endPos))((grid, pos) => {
    grid occupy pos
  })

  println(grid)
  println(count(grid occupy startPos, startPos))
}
