/**
  Car rental game based on what we have in Chapter 4. This generates Figure 4.2
  and helps with the homework assignments there.
  */
package io.samritchie.rl
package world

import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical

object CarRental {
  import Util.Poisson
  import Poisson.Lambda

  val firstLoc = Lambda(3)
  val secondLoc = Lambda(2)

  case class Inventory(n: Int, maxN: Int) {
    def plus(m: Move): Inventory = Inventory(Util.confine(n, 0, maxN), maxN)
  }
  case class Move(n: Int) extends AnyVal

  // One of these comes in for each location.
  case class Update(rentalRequests: Int, returns: Int)
  case class PoissonConfig(upperBound: Int, mean: Lambda)
  case class Location(
      requests: PoissonConfig,
      returns: PoissonConfig
  )

  // This is the update that comes in for both bullshits
  type BigUpdate = (Update, Update)

  case class Config(
      aConfig: Location, // ((11, 3), (11, 3))
      bConfig: Location, // ((11, 4), (11, 2))

      // 20; max number of cars at a location.
      maxCars: Int,
      // 5 max number of cars the user is allowed to move
      maxMoves: Move,
      // 10, credits earned per car
      rentalCredit: Double,
      // cost of moving a car.
      moveCost: Double
  ) {
    def build(a: Inventory, b: Inventory): CarRental = {
      val dist = activityDistribution(aConfig)
        .zip(activityDistribution(bConfig))
      CarRental(dist, a, b)
    }

    def stateSweep(maxA: Inventory, maxB: Inventory): Traversable[CarRental] =
      for {
        a <- (0 until maxA.n)
        b <- (0 until maxB.n)
      } yield build(Inventory(a, maxA.n), Inventory(b, maxB.n))
  }

  // Car Rental location info. This gets me the distribution of requests and returns
  // that show up per location.
  def activityDistribution(config: Location): Categorical[Update] = {
    val Location(requests, returns) = config
    Poisson
      .categorical(requests.upperBound, requests.mean)
      .zip(Poisson.categorical(returns.upperBound, returns.mean))
      .map(Update.tupled)
  }
}

import CarRental.{Inventory, Move}

case class CarRental(
    pmf: Categorical[(CarRental.Update, CarRental.Update)],
    a: CarRental.Inventory,
    b: CarRental.Inventory
) extends State[Move, (Inventory, Inventory), Double, Categorical] {

  val observation: (Inventory, Inventory) = (a, b)

  def dynamics[O2 >: (Inventory, Inventory)]
      : Map[Move, Categorical[(Double, State[Move, O2, Double, Categorical])]] =
    Util.makeMapUnsafe((0 to 10).map(Move(_))) { move =>
      Categorical(Map((10.0, CarRental(pmf, a, b)) -> Real.one))
    }

  /**
    Go through all possibilities...
    */
  def expectedReturn(action: Int, stateValue: Double): Double = {
    val x = 10
    ???
  }

  /**
    Same stuff with a constant returned cars value.
    */
  def expectedReturnConstantReturned(action: Int, stateValue: Double): Double = {
    val x = 10
    ???
  }
}
