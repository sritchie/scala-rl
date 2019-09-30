package io.samritchie.rl
package book

import com.stripe.rainier.cats._
import com.stripe.rainier.core.Categorical
import com.stripe.rainier.compute.Real
import io.samritchie.rl.util.Grid
import io.samritchie.rl.value.{Decaying, MapValueFunction}
import org.scalatest.FunSuite

/**
  * And this is a placeholder for basic tests.
  */
class Chapter4Spec extends FunSuite {
  import Grid.{Move, Position}

  val gamma = 1.0
  val epsilon = 1e-3
  val zeroValue = Decaying(0.0, gamma)
  val expectedFourOne = MapValueFunction(
    Map(
      Position.of(0, 0) -> Real(0.0),
      Position.of(0, 1) -> Real(-13.9989),
      Position.of(0, 2) -> Real(-19.9984),
      Position.of(0, 3) -> Real(-21.9982),
      Position.of(1, 0) -> Real(-13.9989),
      Position.of(1, 1) -> Real(-17.9986),
      Position.of(1, 2) -> Real(-19.9984),
      Position.of(1, 3) -> Real(-19.9984),
      Position.of(2, 0) -> Real(-19.9984),
      Position.of(2, 1) -> Real(-19.9984),
      Position.of(2, 2) -> Real(-17.9986),
      Position.of(2, 3) -> Real(-13.9989),
      Position.of(3, 0) -> Real(-21.9982),
      Position.of(3, 1) -> Real(-19.9984),
      Position.of(3, 2) -> Real(-13.9989),
      Position.of(3, 3) -> Real(0.0)
    ).mapValues(value.Decaying(_, gamma)),
    zeroValue
  )

  test("Figure 4.1's value function matches the gold set") {
    val (actual, _) = Chapter4.fourOne(inPlace = false)
    assert(ValueFunction.diff(actual, expectedFourOne, epsilon)(_.max(_)))
  }

  test("Figure 4.1's calculation matches the full categorical version") {
    val idToCat = Util.idToMonad[Categorical]

    // Empty value function to start.
    val emptyFn = value.Bellman[Position](Map.empty, zeroValue)

    val (actual, _) = ValueFunction.sweepUntil[Move, Position, Double, Categorical, Categorical](
      policy.Random.categorical[Move, Double],
      emptyFn,
      Chapter4.gridConf.stateSweep.map(_.mapK(idToCat)),
      Chapter4.shouldStop _,
      inPlace = true
    )
    assert(ValueFunction.diff(actual, expectedFourOne, epsilon)(_.max(_)))
  }
}
