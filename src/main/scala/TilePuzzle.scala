object TilePuzzle extends App {
  def count(grid: Grid, startPos: Position): Int = {
    def count0(grid: Grid, currentPos: Position): Int = {
      if (grid.solved) 1
      else grid.nextPositions(currentPos).foldLeft(0)((sum, nextPos) => sum + count0(grid occupy nextPos, nextPos))
    }

    count0(grid occupy startPos, startPos)
  }

  def alternativeCount(grid: Grid, startPos: Position): Int = {
    def count0(grid: Grid, currentPos: Position): Int = {
      val next = grid.occupy(currentPos)

      if (next.solved) 1
      else next.nextPositions(currentPos).foldLeft(0)((sum, nextPos) => sum + count0(next, nextPos))
    }

    count0(grid, startPos)
  }

  def solvable(grid: Grid, startPos: Position): Boolean = {
    def solvable0(grid: Grid, currentPos: Position): Boolean = {
      val moved = grid.occupy(currentPos)

      if (moved.solved) true
      else moved.nextPositions(currentPos).exists(nextPos => solvable0(moved, nextPos))
    }

    solvable0(grid, startPos)
  }

  def paths(grid: Grid, startPos: Position): Iterable[Iterable[Position]] = {
    def paths0(grid: Grid, currentPos: Position, path: Vector[Position]): Iterable[Iterable[Position]] = {
      if (grid.solved) Vector(path)
      else grid.nextPositions(currentPos).foldLeft(Vector[Iterable[Position]]())((acc, nextPos) => acc ++ paths0(grid occupy nextPos, nextPos, path :+ nextPos))
    }

    paths0(grid occupy startPos, startPos, Vector(startPos))
  }

  def path(grid: Grid, startPos: Position): Option[Iterable[Position]] = {
    def path0(grid: Grid, currentPos: Position, path: Vector[Position]): Option[Iterable[Position]] = {
      if (grid.solved) Some(path)
      else {
        grid.nextPositions(currentPos).foreach(nextPos => {
          path0(grid occupy nextPos, nextPos, path :+ nextPos) match {
            case path: Some[_] => return path // return early if found
            case _ =>
          }
        })

        None
      }
    }

    path0(grid occupy startPos, startPos, Vector(startPos))
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

  println(new java.util.Date)

  val solutions = paths(grid, startPos)
  println("solutions: " + solutions.size)
  solutions.take(3) foreach println

  println(new java.util.Date)
}
