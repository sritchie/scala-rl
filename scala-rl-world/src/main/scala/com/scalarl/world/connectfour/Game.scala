package com.scalarl.world.connectfour

import scala.util.{Failure, Success, Try}

/**
  * This object implements the game logic for Connect Four. IO.scala
  * implements the actual IO and the game loop.
  */
object Game {

  /**
    * Convenient type aliases for use below.
    */
  type Row = Int
  type Column = Int

  /**
    * Represents the color of a game piece. Board positions are
    * represented with Option[Color.]
    */
  sealed trait Color extends Product with Serializable

  object Color {
    final case object Red extends Color
    final case object Black extends Color

    /**
      * Returns the... other color. Useful for alternating turns.
      */
    def other(color: Color): Color = color match {
      case Red   => Black
      case Black => Red
    }

    /**
      * Prints a representation of the board position to use when
      * printing the entire board out for the user.
      */
    def toString(opt: Option[Color]): String =
      opt match {
        case None        => "-"
        case Some(Red)   => "x"
        case Some(Black) => "o"
      }
  }

  /**
    * Represents a user-generated attempt to drop a piece of `color`
    * onto the column `column`.
    */
  case class Move(column: Column, color: Color)

  /**
    * Class that tracks the state of a current board, along with its
    * dimensions. The winning streak length can also be parameterized.
    */
  class Board(pieces: Array[Option[Color]], width: Int, height: Int, winningStreakLength: Int) {
    import Board.Position

    val maxRow: Row = height - 1
    val maxColumn: Column = width - 1

    private def posToIdx(pos: Position): Int =
      pos.row * width + pos.column

    private def idxToPos(idx: Int): Position =
      Position(idx / width, idx % width)

    /**
      * Returns true if the position lies within the bounds of this
      * board, false otherwise.
      */
    private def isPositionValid(pos: Position): Boolean = {
      val validRow = pos.row >= 0 && pos.row < height
      val validCol = pos.column >= 0 && pos.column < width
      validRow && validCol
    }

    /**
      * Unsafe because it doesn't check if another piece already
      * exists at the supplied position.
      */
    private def updatePieceUnsafe(pos: Position, color: Color): Board =
      new Board(pieces.updated(posToIdx(pos), Some(color)), width, height, winningStreakLength)

    /**
      * Returns None if no piece has been placed at that position (or
      * if the position is invalid and lies outside the bounds of the
      * board!), Some(Color) otherwise.
      *
      * This returns None for invalid positions since this is part of
      * the public API and I'd rather set up that contract than thread
      * a Try through.
      */
    def pieceAt(pos: Position): Option[Color] =
      pieces.lift(posToIdx(pos)).flatten

    /**
      * A move is valid if its column is valid (ie it can generate a
      * valid position) and the new initial position, at the top of
      * the column, is currently empty.
      */
    def isMoveValid(move: Move): Boolean = {
      val pos = Position(maxRow, move.column)
      isPositionValid(pos) && pieceAt(pos).isEmpty
    }

    /**
      * Takes a move and fails out if the move is invalid; else
      * performs the supplied function on the move.
      */
    def tryMove[A](move: Move)(f: Move => A): Try[A] = {
      val topOfColumn = Position(maxRow, move.column)
      if (!isPositionValid(topOfColumn)) {
        Failure(new RuntimeException("The proposed position is invalid."))
      } else if (!pieceAt(topOfColumn).isEmpty) {
        Failure(new RuntimeException("There's already a piece at that position.."))
      } else Success(f(move))
    }

    /**
      * Apply the supplied move to the board and return a pair of (new
      * board, the updated position). This function assumes that the
      * move is valid and throws if not. use tryMove if you need error
      * handling.
      */
    def performMove(move: Move): (Board, Position) = {
      assert(isMoveValid(move), "Move is invalid!")

      // Returns position on the board to update with the supplied
      // move, guaranteed to be valid since isMoveValid only returns
      // true if at least the top entry in the column is empty.
      def loop(pos: Position, remaining: List[Position]): Position =
        (pieceAt(pos), remaining) match {
          case (None, h :: t) => loop(h, t) // empty spot with remaining positions below.
          case (None, Nil)    => pos // Bottom of a column; place piece in this position.
          case (Some(_), _)   => pos.up // We've hit some other piece. Place a piece above.
        }

      val h :: t = (0 to maxRow).reverse
        .map(Position(_, move.column))
        .toList

      val newPos = loop(h, t)
      (updatePieceUnsafe(newPos, move.color), newPos)
    }

    /**
      * Returns true if there are no more possible moves, false
      * otherwise.
      */
    def isFull: Boolean = pieces.forall(!_.isEmpty)

    /**
      * Returns a list of potential lists of positions that, if
      * they all contain the same color, could result in a win.
      *
      * I'm searching in all directions, though I think I could in
      * fact skip searching downRight and downLeft depending on the
      * order in which I check positions, since lower rows will have
      * checked those lines first. I'm keeping them in since I don't
      * have tests here yet.
      */
    private def potentialStreaks(startingPosition: Position): List[List[Position]] = {
      val ops: List[Position => Position] = List(
        _.right,
        _.left,
        _.down,
        _.downRight,
        _.downLeft,
        _.upRight,
        _.upLeft
      )
      ops.map(startingPosition.streak(winningStreakLength)(_))
    }

    private def checkStreakForWin(streak: List[Position]): Option[Color] =
      if (streak.forall(isPositionValid(_))) {
        val pieces = streak.map(pieceAt(_)).distinct
        if (pieces.size == 1) pieces.head else None
      } else None

    /**
      * Checks for wins at any point. If there's no win, returns a
      * None; else returns Some(the color of the winning side).
      *
      * I could make this more efficient by only checking for winning
      * streaks created by the most recent move, but this is fine with
      * smaller boards, including the actual game's settings.
      *
      * I implemented it this way to catch cases where the winning
      * move completes a streak in the middle.
      */
    def checkAllPositionsForWin: Option[Color] =
      (0 to (width * height))
        .map(idxToPos(_))
        .flatMap(potentialStreaks(_))
        .map(checkStreakForWin(_))
        .find(_.isDefined)
        .flatten

    // returns the current string representation of the board.
    override def toString: String = {
      val numberRow = (0 to maxColumn).mkString(" ")
      val gameRows = (0 to maxRow).reverse.map { row =>
        (0 to maxColumn)
          .map { col =>
            val pos = Position(row, col)
            Color.toString(pieceAt(pos))
          }
          .mkString(" ")
      }
      (numberRow +: gameRows).mkString("\n")
    }
  }

  object Board {

    /**
      * Class representing a position on the board.
      */
    case class Position(row: Row, column: Column) {

      /**
        * Returns a list of positions achieved by walking the position
        * using the supplied function.
        */
      def streak(n: Int)(f: Position => Position): List[Position] =
        Iterator.iterate(this)(f).take(n).toList

      def up: Position = Position(row + 1, column)
      def down: Position = Position(row - 1, column)
      def right: Position = Position(row, column + 1)
      def left: Position = Position(row, column - 1)
      def downRight: Position = down.right
      def downLeft: Position = down.left
      def upRight: Position = up.right
      def upLeft: Position = up.left
    }

    /**
      * Returns an empty board instance with the supplied width,
      * height and required streak length to win the game, with all
      * positions initialized to empty.
      */
    def empty(width: Int, height: Int, winningStreakLength: Int): Board = {
      assert(width > 0, "Width must be greater than 0.")
      assert(height > 0, "Height must be greater than 0.")
      assert(winningStreakLength > 2, "Winning streak must be greater than 2.")

      new Board(Array.fill(width * height)(None), width, height, winningStreakLength)
    }

    /**
      * Creates a default connect four board with the parameters of
      * the board game.
      */
    def defaultEmpty: Board = empty(7, 6, 4)
  }
}
