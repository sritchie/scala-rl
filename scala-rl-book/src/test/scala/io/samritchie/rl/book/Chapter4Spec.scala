package io.samritchie.rl
package book

import io.samritchie.rl.util.Grid
import io.samritchie.rl.value.{Bellman, Decaying}
import org.scalatest.FunSuite

/**
  * And this is a placeholder for basic tests.
  */
class Chapter4Spec extends FunSuite {
  import Grid.{Move, Position}

  val gamma = 1.0
  val epsilon = 1e-3
  val zeroValue = Decaying(0.0, gamma)
  val expectedFourOne = Bellman(
    Map(
      Position.of(0, 0) -> 0.0,
      Position.of(0, 1) -> -13.9989,
      Position.of(0, 2) -> -19.9984,
      Position.of(0, 3) -> -21.9982,
      Position.of(1, 0) -> -13.9989,
      Position.of(1, 1) -> -17.9986,
      Position.of(1, 2) -> -19.9984,
      Position.of(1, 3) -> -19.9984,
      Position.of(2, 0) -> -19.9984,
      Position.of(2, 1) -> -19.9984,
      Position.of(2, 2) -> -17.9986,
      Position.of(2, 3) -> -13.9989,
      Position.of(3, 0) -> -21.9982,
      Position.of(3, 1) -> -19.9984,
      Position.of(3, 2) -> -13.9989,
      Position.of(3, 3) -> 0.0
    ).mapValues(value.Decaying(_, gamma)),
    zeroValue
  )

  test("Figure 4.1's value function matches the gold set") {
    val (actual, _) = Chapter4.fourOne(inPlace = false)
    assert(ValueFunction.diffBelow(actual, expectedFourOne, epsilon)(_.max(_)))
  }

  test("Figure 4.1's calculation matches the full categorical version") {
    val idToCat = Util.idToMonad[Cat]

    // Empty value function to start.
    val emptyFn = value.Bellman[Position](Map.empty, zeroValue)

    val (actual, _) = ValueFunction.sweepUntil[Move, Position, Double, Cat, Cat](
      emptyFn,
      _ => policy.Random.cat[Move, Position, Double],
      Chapter4.gridConf.stateSweep.map(_.mapK(idToCat)),
      Chapter4.shouldStop(_, _, _),
      inPlace = true,
      valueIteration = true
    )
    assert(ValueFunction.diffBelow(actual, expectedFourOne, epsilon)(_.max(_)))
  }
}
