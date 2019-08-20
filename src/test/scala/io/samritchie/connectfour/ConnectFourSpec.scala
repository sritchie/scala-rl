package io.samritchie.connectfour

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

/**
  * I haven't written any tests yet, of course. This is a placeholder
  * for laws about Connect Four.
  */
object ConnectFourSpec extends Properties("ConnectFour") with ConnectFourArb {
  import Game._

  def makeMoveValid(move: Move, maxColumns: Int): Move =
    move.copy(
      column = if (maxColumns == 0) 0 else move.column % maxColumns
    )

  property("generated moves are valid.") = forAll {
    (move: Move, columns: Int) =>
    val posColumns = Math.abs(columns % 100)
    makeMoveValid(move, posColumns).column <= posColumns
  }
}

/**
  * And this is a placeholder for basic tests.
  */
class ConnectFourTest extends org.scalatest.FunSuite {
  test("example.test") {
    val digits = List(1,2,3)
    assert(digits.sum === 6)
  }
}


/**
  * Generators and Arbitrary instances live below.
  */
trait ConnectFourGen {
  import Game._

  implicit val genColor: Gen[Color] = Gen.oneOf(Red, Black)

  implicit val genMove: Gen[Move] = for {
    column <- Gen.posNum[Int]
    color <- genColor
  } yield Move(column, color)
}

object ConnectFourGenerators extends ConnectFourGen

trait ConnectFourArb {
  import Game._
  import ConnectFourGenerators._

  implicit val arbColor: Arbitrary[Color] = Arbitrary(genColor)
  implicit val arbMove: Arbitrary[Move] = Arbitrary(genMove)
}
