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
class Chapter3Spec extends FunSuite {
  import Grid.{Move, Position}

  val gamma = 0.9
  val epsilon = 1e-4
  val zeroValue = Decaying(0.0, gamma)

  test("Figure 3.2's value function matches the gold set") {
    val (actual, _) = Chapter3.threeTwo
    val expected = MapValueFunction(
      Map(
        Position.of(0, 0) -> Real(3.3090),
        Position.of(0, 1) -> Real(8.7893),
        Position.of(0, 2) -> Real(4.4276),
        Position.of(0, 3) -> Real(5.3223),
        Position.of(0, 4) -> Real(1.4921),
        Position.of(1, 0) -> Real(1.5216),
        Position.of(1, 1) -> Real(2.9923),
        Position.of(1, 2) -> Real(2.2501),
        Position.of(1, 3) -> Real(1.9075),
        Position.of(1, 4) -> Real(0.5474),
        Position.of(2, 0) -> Real(0.0508),
        Position.of(2, 1) -> Real(0.7381),
        Position.of(2, 2) -> Real(0.6731),
        Position.of(2, 3) -> Real(0.3582),
        Position.of(2, 4) -> Real(-0.4031),
        Position.of(3, 0) -> Real(-0.9735),
        Position.of(3, 1) -> Real(-0.4354),
        Position.of(3, 2) -> Real(-0.3548),
        Position.of(3, 3) -> Real(-0.5855),
        Position.of(3, 4) -> Real(-1.1830),
        Position.of(4, 0) -> Real(-1.8576),
        Position.of(4, 1) -> Real(-1.3452),
        Position.of(4, 2) -> Real(-1.2292),
        Position.of(4, 3) -> Real(-1.4229),
        Position.of(4, 4) -> Real(-1.9751)
      ).mapValues(Decaying(_, gamma)),
      zeroValue
    )

    assert(ValueFunction.diff(actual, expected, epsilon)(_.max(_)))
  }

  val expectedThreeFive = MapValueFunction(
    Map(
      Position.of(0, 0) -> Real(19.8896),
      Position.of(0, 1) -> Real(24.4194),
      Position.of(0, 2) -> Real(21.9774),
      Position.of(0, 3) -> Real(19.4194),
      Position.of(0, 4) -> Real(17.4774),
      Position.of(1, 0) -> Real(19.7797),
      Position.of(1, 1) -> Real(21.9774),
      Position.of(1, 2) -> Real(19.7797),
      Position.of(1, 3) -> Real(17.8017),
      Position.of(1, 4) -> Real(16.0215),
      Position.of(2, 0) -> Real(17.8017),
      Position.of(2, 1) -> Real(19.7797),
      Position.of(2, 2) -> Real(17.8017),
      Position.of(2, 3) -> Real(16.0215),
      Position.of(2, 4) -> Real(14.4194),
      Position.of(3, 0) -> Real(16.0215),
      Position.of(3, 1) -> Real(17.8017),
      Position.of(3, 2) -> Real(16.0215),
      Position.of(3, 3) -> Real(14.4194),
      Position.of(3, 4) -> Real(12.9774),
      Position.of(4, 0) -> Real(14.4194),
      Position.of(4, 1) -> Real(16.0215),
      Position.of(4, 2) -> Real(14.4194),
      Position.of(4, 3) -> Real(12.9774),
      Position.of(4, 4) -> Real(11.6797)
    ).mapValues(Decaying(_, gamma)),
    zeroValue
  )

  test("Figure 3.5's value function matches the gold set.") {
    val (actual, _) = Chapter3.threeFive
    assert(ValueFunction.diff(actual, expectedThreeFive, epsilon)(_.max(_)))
  }

  test("Figure 3.5's calculation matches the full categorical version") {
    val idToCat = Util.idToMonad[Categorical]

    // Empty value function to start.
    val emptyFn = value.Bellman[Position](Map.empty, zeroValue)

    // Build a Stochastic version of the greedy policy.
    val stochasticConf = policy.Greedy.Config[Double](0.0, zeroValue)

    val (actual, _) = ValueFunction.sweepUntil[Move, Position, Double, Categorical, Categorical](
      emptyFn,
      stochasticConf.stochastic[Move, Position](_),
      Chapter3.gridConf.stateSweep.map(_.mapK(idToCat)),
      Chapter3.shouldStop _,
      inPlace = true
    )

    assert(ValueFunction.diff(actual, expectedThreeFive, epsilon)(_.max(_)))
  }
}
