/**
  * Grid-related utilities. I bet I could generate 1, 2, 3d grids,
  * with custom moves between them...
  */
package io.samritchie.rl
package util

import scala.util.{Failure, Success, Try}

object Grid {
  sealed trait Move
  object Move {
    case object Left extends Move
    case object Right extends Move
    case object Up extends Move
    case object Down extends Move

    val all: Set[Move] = Set(Left, Right, Up, Down)
  }

  case class Row(value: Int) extends AnyVal {
    def up: Row = Row(value + 1)
    def down: Row = Row(value - 1)

    /**
      * Returns a row that's guaranteed to sit within the range
      * specified by numColumns.
      */
    def confine(numRows: Int): Row =
      Row(Util.confine(value, 0, numRows - 1))

    def isWithin(numRows: Int): Boolean = value >= 0 && value < numRows
    def assertWithin(numRows: Int): Try[Row] =
      if (isWithin(numRows))
        Success(this)
      else
        Failure(
          new AssertionError(
            s"Column $value is invalid: Must be between 0 and $numRows."
          )
        )
  }

  case class Col(value: Int) extends AnyVal {
    def left: Col = Col(value + 1)
    def right: Col = Col(value - 1)

    /**
      * Returns a column that's guaranteed to sit within the range
      * specified by numColumns.
      */
    def confine(numColumns: Int): Col =
      Col(Util.confine(value, 0, numColumns - 1))

    def isWithin(numColumns: Int): Boolean = value >= 0 && value < numColumns
    def assertWithin(numColumns: Int): Try[Col] =
      if (isWithin(numColumns))
        Success(this)
      else
        Failure(
          new AssertionError(
            s"Column $value is invalid: Must be between 0 and $numColumns."
          )
        )

  }

  object Position {
    def of(row: Int, col: Int): Position =
      apply(Row(row), Col(col))
  }
  case class Position(row: Row, col: Col) {
    def left: Position = Position(row, col.left)
    def right: Position = Position(row, col.right)
    def up: Position = Position(row.up, col)
    def down: Position = Position(row.down, col)

    def confine(bounds: Bounds): Position =
      Position(
        row.confine(bounds.numRows),
        col.confine(bounds.numColumns)
      )

    def isWithin(bounds: Bounds): Boolean =
      row.isWithin(bounds.numRows) && col.isWithin(bounds.numColumns)

    def assertWithin(bounds: Bounds): Try[Position] =
      for {
        r <- row.assertWithin(bounds.numRows)
        x <- col.assertWithin(bounds.numColumns)
      } yield this
  }
  case class Bounds(numRows: Int, numColumns: Int) {
    def allPositions: Traversable[Position] =
      for {
        r <- (0 until numRows)
        c <- (0 until numColumns)
      } yield Position.of(r, c)
  }

  /**
    * Produces a traversable instance containing all possible Grid
    * states.
    */
  def allStates(bounds: Bounds): Traversable[Grid] =
    bounds.allPositions.map(Grid(_, bounds))
}

case class Grid(position: Grid.Position, bounds: Grid.Bounds) {
  import Grid.{Move, Position}

  private def moveF(move: Move): Position => Position =
    move match {
      case Move.Left  => _.left
      case Move.Right => _.right
      case Move.Up    => _.up
      case Move.Down  => _.down
    }

  def move(move: Move): Try[Grid] =
    teleport(moveF(move)(position))

  def teleportUnsafe(newPosition: Position): Grid =
    copy(position = newPosition)

  def teleport(newPosition: Position): Try[Grid] =
    newPosition.assertWithin(bounds).map(Grid(_, bounds))
}
