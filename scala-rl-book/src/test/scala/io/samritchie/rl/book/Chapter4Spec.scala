package io.samritchie.rl
package book

import io.samritchie.rl.logic.Sweep
import io.samritchie.rl.value.{DecayState, StateValueMap}
import io.samritchie.rl.world.util.Grid
import org.scalatest.funsuite.AnyFunSuite

/**
  * And this is a placeholder for basic tests.
  */
class Chapter4Spec extends AnyFunSuite {
  import Grid.{Move, Position}

  val gamma = 1.0
  val epsilon = 1e-3
  val zeroValue = DecayState.DecayedValue(0.0)
  val expectedFourOne = StateValueMap[Position, DecayState[Double]](
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
    ).mapValues(DecayState.DecayedValue(_)),
    zeroValue
  )

  test("Figure 4.1's value function matches the gold set") {
    val (actual, _) = Chapter4.fourOne(inPlace = false)
    assert(Sweep.diffBelow(actual, expectedFourOne, epsilon)(_.max(_)))
  }

  test("Figure 4.1's calculation matches the full categorical version") {
    val idToCat = Util.idToMonad[Cat]

    // Empty value function to start.
    val emptyFn = value.StateValueMap[Position, DecayState[Double]](Map.empty, zeroValue)

    val (actual, _) = Sweep.sweepUntil[Position, Move, Double, DecayState[Double], Cat, Cat](
      emptyFn,
      _ => Policy.random[Position, Move, Double, Cat],
      DecayState.bellmanFn(gamma),
      Chapter4.gridConf.stateSweep.map(_.mapK(idToCat)),
      Chapter4.shouldStop(_, _, _),
      inPlace = true,
      valueIteration = true
    )
    assert(Sweep.diffBelow(actual, expectedFourOne, epsilon)(_.max(_)))
  }
}
