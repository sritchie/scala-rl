package io.samritchie.connectfour

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object IO {
  import Game._

  /**
   * Return successful piece if it's possible to parse, failure
   * otherwise.
   */
  def getColumn(color: Color): Column = {
    println(s"Enter the column where you'd like to place your $color piece.")
    try {
      StdIn.readLine("column> ").toInt
    } catch {
      case e: NumberFormatException =>
        println("What you entered isn't a valid number. Try again.")
        println
        getColumn(color)
    }
  }

  /**
   * Gets the starting game color from the user.
   */
  def initialColor: Color = {
    println("What color would like to go first? Red's the default.")
    StdIn.readLine("red or black> ").toLowerCase match {
      case "" =>
        println("You entered nothing. We'll default to red.")
        println
        Red
      case "red" =>
        println("Red it is!")
        println
        Red
      case "black" =>
        println("Black it is!")
        println
        Red
      case _ =>
        println("I'm afraid that's not a valid input. Try again!")
        println
        initialColor
    }
  }

  /**
   * Prints out the current board with some surrounding text.
   */
  def printBoardState(board: Board): Unit = {
    println("Current Board State:")
    println(board)
    println
  }

  /**
   * Performs a turn and returns either a failure, if the move was
   * invalid in some way, or a successful pair of the new board and
   * the position updated by the move.
   */
  def turn(board: Board, turnColor: Color): Try[(Board, Board.Position)] = {
    printBoardState(board)
    val column = getColumn(turnColor)
    board.tryMove(Move(column, turnColor))(board.performMove(_))
  }

  /**
   * Plays the game to completion, looping on every turn and
   * alternating colors. The game ends if the board fills up or if
   * one side wins.
   */
  def gameLoop(board: Board, turnColor: Color): Unit =
    turn(board, turnColor) match {
      case Success((newBoard, position)) =>
        println(s"Nice, piece placed at $position.")
        println

        newBoard.checkAllPositionsForWin match {
          case None =>
            if (newBoard.isFull) {
              println("Oh no, the board is full! Thanks for playing.")
              System.exit(0)

            } else {
              gameLoop(newBoard, Color.other(turnColor))
            }
          case Some(winningColor) =>
            println(s"Congratulations, $winningColor... You win!")
            printBoardState(newBoard)
            System.exit(0)
        }

      case Failure(e) =>
        println("Whoops, your move was invalid with the following error: ")
        println(e.getMessage)
        println("Let's try that again.")
        println

        gameLoop(board, turnColor)
    }

  def main(items: Array[String]): Unit =
    gameLoop(Board.defaultEmpty, initialColor)
}
