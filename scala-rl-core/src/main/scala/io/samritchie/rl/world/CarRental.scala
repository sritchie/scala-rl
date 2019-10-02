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

  case class Inventory(n: Int, maxN: Int) {
    def -(m: Move): Inventory = this + -m
    def +(m: Move): Inventory = Inventory(Util.confine(n + m.n, 0, maxN), maxN)
  }
  case class Move(n: Int) extends AnyVal {
    def unary_- = Move(-n)
  }
  object Move {
    def inclusiveRange(fromMove: Move, toMove: Move): Iterable[Move] =
      (fromMove.n to toMove.n).map(Move(_))
  }

  // One of these comes in for each location.
  case class Update(rentalRequests: Int, returns: Int)

  sealed trait DistConf
  case class PoissonConfig(upperBound: Int, mean: Lambda) extends DistConf
  case class ConstantConfig(mean: Int) extends DistConf
  case class Location(
      requests: DistConf,
      returns: DistConf,
      maxCars: Int
  )

  // This is the update that comes in for both bullshits
  type InvPair = (Inventory, Inventory)

  case class Config(
      aConfig: Location,
      bConfig: Location,
      maxMoves: Move,
      rentalCredit: Double,
      moveCost: Double
  ) {
    val allMoves: Iterable[Move] = Move.inclusiveRange(-maxMoves, maxMoves)
    lazy val dist = activityDistribution(aConfig)
      .zip(activityDistribution(bConfig))

    def build(a: Inventory, b: Inventory): CarRental =
      CarRental(this, dist, a, b)

    def stateSweep: Traversable[CarRental] =
      for {
        a <- (0 to aConfig.maxCars)
        b <- (0 to bConfig.maxCars)
      } yield build(Inventory(a, aConfig.maxCars), Inventory(b, bConfig.maxCars))
  }

  def toDistribution(config: DistConf): Categorical[Int] =
    config match {
      case PoissonConfig(upperBound, mean) =>
        Poisson.categorical(upperBound, mean)
      case ConstantConfig(mean) => Categorical(Map(mean -> Real.one))
    }

  // Car Rental location info. This gets me the distribution of requests and
  // returns that show up per location.
  def activityDistribution(config: Location): Categorical[Update] =
    toDistribution(config.requests)
      .zip(toDistribution(config.returns))
      .map(Update.tupled)
}

import CarRental.{Inventory, Move, Update}

case class CarRental(
    config: CarRental.Config,
    pmf: Categorical[(Update, Update)],
    a: Inventory,
    b: Inventory
) extends State[Move, (Inventory, Inventory), Double, Categorical] {

  val observation: (Inventory, Inventory) = (a, b)

  /**
      Go through all possibilities...

    FIRST move the cars.
    THEN calculate the cost.

    THEN do the Poisson update and factor in the amount of money back, plus
    costs...

    positive goes from a to b, negative goes from b to a.
    */
  def dynamics[O2 >: (Inventory, Inventory)]
      : Map[Move, Categorical[(Double, State[Move, O2, Double, Categorical])]] =
    // TODO filter this so that we don't present moves that will more than
    // deplete some spot. Overloading is fine, since it gets the cars off the
    // board... I guess?
    Util.makeMapUnsafe(config.allMoves) { move =>
      pmf.map {
        case (aUpdate, bUpdate) =>
          val (newA, newB, reward) = processAll(move, aUpdate, bUpdate)
          (reward, copy(a = newA, b = newB))
      }
    }

  private def processAll(
      move: Move,
      aUpdate: Update,
      bUpdate: Update
  ): (Inventory, Inventory, Double) = {
    // TODO this shouldn't charge you if you CAN'T move a car. Check if there
    // are enough and fix that.
    val moveCost = config.moveCost * math.abs(move.n)
    val (newA, rewardA) = process(-move, a, aUpdate)
    val (newB, rewardB) = process(move, b, bUpdate)
    (newA, newB, rewardA + rewardB - moveCost)
  }

  private def process(move: Move, inventory: Inventory, update: Update): (Inventory, Double) = {
    val afterMove = inventory + move
    val validRentals = math.min(afterMove.n, update.rentalRequests)
    val nextInventory = afterMove - Move(validRentals) + Move(update.returns)
    (nextInventory, config.rentalCredit * validRentals)
  }
}
