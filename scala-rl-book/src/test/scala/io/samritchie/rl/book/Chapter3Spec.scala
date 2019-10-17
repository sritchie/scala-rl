package io.samritchie.rl
package book

import io.samritchie.rl.logic.Sweep
import io.samritchie.rl.util.Grid
import io.samritchie.rl.value.{Decaying, StateValueMap}
import org.scalatest.FunSuite

/**
  * And this is a placeholder for basic tests.
  */
class Chapter3Spec extends FunSuite {
  import Grid.{Move, Position}

  val gamma = 0.9
  val epsilon = 1e-4
  val zeroValue = Decaying(0.0, gamma)

  test("Figure 3.2's value function matches the gold set") {
    val (actual, _) = Chapter3.threeTwo
    val expected = StateValueMap(
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
      ).mapValues(Decaying(_, gamma)),
      zeroValue
    )

    assert(StateValueFn.diffBelow(actual, expected, epsilon)(_.max(_)))
  }

  val expectedThreeFive = StateValueMap(
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
    ).mapValues(Decaying(_, gamma)),
    zeroValue
  )

  test("Figure 3.5's value function matches the gold set.") {
    val (actual, _) = Chapter3.threeFive
    assert(StateValueFn.diffBelow(actual, expectedThreeFive, epsilon)(_.max(_)))
  }

  test("Figure 3.5's calculation matches the full categorical version") {
    val idToCat = Util.idToMonad[Cat]

    // Empty value function to start.
    val emptyFn = StateValueFn[Position](zeroValue)

    // Build a Stochastic version of the greedy policy.
    val stochasticConf = policy.Greedy.Config[Double](0.0, zeroValue)

    val (actual, _) = Sweep.sweepUntil[Position, Move, Double, Cat, Cat](
      emptyFn,
      stochasticConf.stochastic[Position, Move](_),
      (vf, p) => Estimator.bellman(vf, p, zeroValue, zeroValue),
      Chapter3.gridConf.stateSweep.map(_.mapK(idToCat)),
      Chapter3.shouldStop _,
      inPlace = true,
      valueIteration = true
    )
    assert(StateValueFn.diffBelow(actual, expectedThreeFive, epsilon)(_.max(_)))
  }
}
