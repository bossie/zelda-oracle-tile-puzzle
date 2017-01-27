package org.bossie.zotp.grid

import scala.annotation.tailrec

case class GridConfiguration(val rows: Int, val columns: Int, start: Position, occupieds: Iterable[Position]) {
  require(rows > 0)
  require(columns > 0)
}

object GridConfiguration {
  def fromString(s: String): GridConfiguration = {
    @tailrec
    def fromString0(cs: List[Char], row: Int, col: Int, startPos: Position, occupieds: List[Position]): GridConfiguration = {
      cs match {
        case Nil => new GridConfiguration(row + 1, col, startPos, occupieds) // an EOF results in a new row
        case y :: ys => y match {
          case 's' => fromString0(ys, row, col + 1, Position(row, col), occupieds)
          case '#' => fromString0(ys, row, col + 1, startPos, Position(row, col) :: occupieds)
          case '\n' => fromString0(ys, row + 1, 0, startPos, occupieds)
          case _ => fromString0(ys, row, col + 1, startPos, occupieds)
        }
      }
    }

    fromString0(s.toList, 0, 0, Position(0, 0), Nil) // TODO: what happens if the last character is a newline?
  }
}
