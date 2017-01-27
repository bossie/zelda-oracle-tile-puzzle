package org.bossie.zotp

import grid.{Grid, GridConfigurations}

object TilePuzzle extends App {
  val config = GridConfigurations.ooa3

  val emptyGrid = new Grid(config.rows, config.columns)
  val occupieds = config.occupieds
  val startPos = config.start

  val grid = occupieds.foldLeft(emptyGrid)((grid, pos) => grid occupy pos)

  println(grid)

  def seconds(f: => Unit): Long = {
    val start = System.currentTimeMillis()
    f
    (System.currentTimeMillis() - start) / 1000
  }

  println(s"count: ${seconds(Recursive.count(grid, startPos))}s")
  println(s"alternative: ${seconds(Recursive.alternativeCount(grid, startPos))}s")
  println(s"fork/join: ${seconds(ForkJoin.count(grid, startPos))}s")
}
