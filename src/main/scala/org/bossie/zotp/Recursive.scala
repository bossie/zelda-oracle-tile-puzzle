package org.bossie.zotp

import org.bossie.zotp.grid.{Grid, Position}

object Recursive {

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

  def pathsWithList(grid: Grid, startPos: Position): Iterable[Iterable[Position]] = {
    def pathsWithList0(grid: Grid, currentPos: Position, path: List[Position]): List[Iterable[Position]] = {
      if (grid.solved) List(path.reverse)
      else grid.nextPositions(currentPos).foldLeft(List[Iterable[Position]]())((acc, nextPos) => acc ::: pathsWithList0(grid occupy nextPos, nextPos, nextPos :: path))
    }

    pathsWithList0(grid occupy startPos, startPos, List(startPos))
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
}
