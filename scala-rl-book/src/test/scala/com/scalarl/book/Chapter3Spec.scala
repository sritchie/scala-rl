package com.scalarl
package book

import com.scalarl.logic.Sweep
import com.scalarl.value.DecayState
import com.scalarl.world.util.Grid
import org.scalatest.funsuite.AnyFunSuite

/**
  * And this is a placeholder for basic tests.
  */
class Chapter3Spec extends AnyFunSuite {
  import Grid.{Move, Position}

  val epsilon = 1e-4
  val gamma = 0.9
  val zeroValue = DecayState.DecayedValue(0.0)

  test("Figure 3.2's value function matches the gold set") {
    val (actual, _) = Chapter3.threeTwo
    val expected = StateValueFn.Base[Position, DecayState[Double]](
      Map(
        Position.of(0, 0) -> 3.3090,
        Position.of(0, 1) -> 8.7893,
        Position.of(0, 2) -> 4.4276,
        Position.of(0, 3) -> 5.3223,
        Position.of(0, 4) -> 1.4921,
        Position.of(1, 0) -> 1.5216,
        Position.of(1, 1) -> 2.9923,
        Position.of(1, 2) -> 2.2501,
        Position.of(1, 3) -> 1.9075,
        Position.of(1, 4) -> 0.5474,
        Position.of(2, 0) -> 0.0508,
        Position.of(2, 1) -> 0.7381,
        Position.of(2, 2) -> 0.6731,
        Position.of(2, 3) -> 0.3582,
        Position.of(2, 4) -> -0.4031,
        Position.of(3, 0) -> -0.9735,
        Position.of(3, 1) -> -0.4354,
        Position.of(3, 2) -> -0.3548,
        Position.of(3, 3) -> -0.5855,
        Position.of(3, 4) -> -1.1830,
        Position.of(4, 0) -> -1.8576,
        Position.of(4, 1) -> -1.3452,
        Position.of(4, 2) -> -1.2292,
        Position.of(4, 3) -> -1.4229,
        Position.of(4, 4) -> -1.9751
      ).mapValues(DecayState.DecayedValue(_)),
      zeroValue
    )

    assert(Sweep.diffBelow(actual, expected, epsilon)(_.max(_)))
  }

  val expectedThreeFive = StateValueFn.Base[Position, DecayState[Double]](
    Map(
      Position.of(0, 0) -> 21.9774,
      Position.of(0, 1) -> 24.4194,
      Position.of(0, 2) -> 21.9774,
      Position.of(0, 3) -> 19.4194,
      Position.of(0, 4) -> 17.4774,
      Position.of(1, 0) -> 19.7797,
      Position.of(1, 1) -> 21.9774,
      Position.of(1, 2) -> 19.7797,
      Position.of(1, 3) -> 17.8017,
      Position.of(1, 4) -> 16.0215,
      Position.of(2, 0) -> 17.8017,
      Position.of(2, 1) -> 19.7797,
      Position.of(2, 2) -> 17.8017,
      Position.of(2, 3) -> 16.0215,
      Position.of(2, 4) -> 14.4194,
      Position.of(3, 0) -> 16.0215,
      Position.of(3, 1) -> 17.8017,
      Position.of(3, 2) -> 16.0215,
      Position.of(3, 3) -> 14.4194,
      Position.of(3, 4) -> 12.9774,
      Position.of(4, 0) -> 14.4194,
      Position.of(4, 1) -> 16.0215,
      Position.of(4, 2) -> 14.4194,
      Position.of(4, 3) -> 12.9774,
      Position.of(4, 4) -> 11.6797
    ).mapValues(DecayState.DecayedValue(_)),
    zeroValue
  )

  test("Figure 3.5's value function matches the gold set.") {
    val (actual, _) = Chapter3.threeFive
    assert(Sweep.diffBelow(actual, expectedThreeFive, epsilon)(_.max(_)))
  }

  test("Figure 3.5's calculation matches the full categorical version") {
    val idToCat = Util.idToMonad[Cat]

    // Empty value function to start.
    val emptyFn = StateValueFn.empty[Position, DecayState[Double]](zeroValue)

    // Build a Stochastic version of the greedy policy.
    implicit val dm = DecayState.decayStateModule(gamma)
    val stochasticConf = policy.Greedy.Config[Double, DecayState[Double]](
      0.0,
      DecayState.Reward(_),
      (a, b) => DecayState.decayStateGroup[Double](gamma).plus(a, b),
      zeroValue
    )

    val (actual, _) = Sweep.sweepUntil[Position, Move, Double, DecayState[Double], Cat, Cat](
      emptyFn,
      stochasticConf.stochastic[Position, Move](_),
      DecayState.bellmanFn(gamma),
      Chapter3.gridConf.stateSweep.map(_.mapK(idToCat)),
      Chapter3.shouldStop _,
      inPlace = true,
      valueIteration = true
    )
    assert(Sweep.diffBelow(actual, expectedThreeFive, epsilon)(_.max(_)))
  }
}
