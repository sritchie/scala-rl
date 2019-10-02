/**
  Car rental game based on what we have in Chapter 4. This generates Figure 4.2
  and helps with the homework assignments there.
  */
package io.samritchie.rl
package world

object CarRental {
  import Cat.Poisson
  import Poisson.Lambda

  case class Inventory(n: Int, maxN: Int) {
    def -(m: Move): Inventory = this + -m
    def +(m: Move): Inventory = Inventory(Util.confine(n + m.n, 0, maxN), maxN)
    def update(rentals: Move, returns: Move): Inventory =
      Inventory(
        math.min(n - rentals.n + returns.n, maxN),
        maxN
      )
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
    import cats.implicits._

    val allMoves: Iterable[Move] = Move.inclusiveRange(-maxMoves, maxMoves)
    lazy val dist: Cat[(Update, Update)] =
      (
        toDistribution(aConfig.requests),
        toDistribution(aConfig.returns),
        toDistribution(bConfig.requests),
        toDistribution(bConfig.returns)
      ).mapN {
        case (a, b, c, d) => (Update(a, b), Update(c, d))
      }

    def build(a: Inventory, b: Inventory): State[Move, (Inventory, Inventory), Double, Cat] =
      CarRental(this, dist, a, b)

    def stateSweep: Traversable[State[Move, (Inventory, Inventory), Double, Cat]] =
      for {
        a <- (0 to aConfig.maxCars)
        b <- (0 to bConfig.maxCars)
      } yield build(Inventory(a, aConfig.maxCars), Inventory(b, bConfig.maxCars))
  }

  def toDistribution(config: DistConf): Cat[Int] =
    config match {
      case PoissonConfig(upperBound, mean) =>
        Cat.poisson(upperBound, mean)
      case ConstantConfig(mean) => Cat.pure(mean)
    }
}

import CarRental.{Inventory, Move, Update}

case class CarRental(
    config: CarRental.Config,
    pmf: Cat[(Update, Update)],
    a: Inventory,
    b: Inventory
) extends State[Move, (Inventory, Inventory), Double, Cat] {

  val observation: (Inventory, Inventory) = (a, b)

  /**
      Go through all possibilities...

    FIRST move the cars.
    THEN calculate the cost.

    THEN do the Poisson update and factor in the amount of money back, plus
    costs...

    positive goes from a to b, negative goes from b to a.

    TODO filter this so that we don't present moves that will more than
    deplete some spot. Overloading is fine, since it gets the cars off the
    board... I guess?
    */
  def dynamics[O2 >: (Inventory, Inventory)]: Map[Move, Cat[(Double, State[Move, O2, Double, Cat])]] =
    fixedDynamics

  // TODO I THINK we can only make this faster if we decide to use an Eval...
  // get an EvalT going for the monads, and define an expected value instance
  // there. But then the first person to go and iterate through will evaluate
  // everything.
  private lazy val fixedDynamics: Map[Move, Cat[(Double, State[Move, (Inventory, Inventory), Double, Cat])]] =
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
    val nextInventory = afterMove.update(Move(validRentals), Move(update.returns))
    (nextInventory, config.rentalCredit * validRentals)
  }
}
