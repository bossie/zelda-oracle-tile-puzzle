package org.bossie.zotp

import org.bossie.zotp.grid.{Grid, Position}
import java.util.concurrent.{ForkJoinPool, RecursiveTask}

// -Djava.util.concurrent.ForkJoinPool.common.parallelism=4
object ForkJoin {

  def count(grid: Grid, startPos: Position): Int = {
    class Count(grid: Grid, currentPos: Position) extends RecursiveTask[Int] {
      override def compute: Int = {
        val next = grid.occupy(currentPos)

        if (next.solved) 1
        else next.nextPositions(currentPos) match {
          case Nil => 0
          case p :: ps => {
            val me = new Count(next, p)
            val others = for (nextPos <- ps) yield new Count(next, nextPos).fork()

            me.compute + others.map(_.join).sum
          }
        }
      }
    }

    ForkJoinPool.commonPool().invoke(new Count(grid, startPos))
  }
}
