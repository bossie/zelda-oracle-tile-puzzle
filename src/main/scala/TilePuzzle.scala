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
