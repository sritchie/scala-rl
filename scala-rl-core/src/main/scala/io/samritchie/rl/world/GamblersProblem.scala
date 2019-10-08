/**
  Gambler's Problem! Chapter 4 again; this generates Figure 4.3.
  */
package io.samritchie.rl
package world

object GamblersProblem {
  case class Amount(p: Int) extends AnyVal {
    def >=(r: Amount): Boolean = p >= r.p
  }

  case class Config(
      headProb: Double,
      winningAmount: Amount,
      winningReward: Double
  ) {
    val headsDistribution: Cat[Boolean] =
      Cat.boolean(headProb)

    def build(startingAmount: Amount): GamblersProblem =
      GamblersProblem(this, startingAmount)

    def stateSweep: Traversable[GamblersProblem] =
      for (amt <- (0 until winningAmount.p)) yield build(Amount(amt))
  }
}

/**
  Gotta read more about what the hell is going on, but the key is that we have
  100 possible states... for the value function.
  */
case class GamblersProblem(
    config: GamblersProblem.Config,
    amount: GamblersProblem.Amount
) extends State[GamblersProblem.Amount, GamblersProblem.Amount, Double, Cat] {
  import GamblersProblem.Amount

  override val observation = amount

  override lazy val dynamics: Map[Amount, Cat[(Double, GamblersProblem)]] =
    if (amount >= config.winningAmount || amount.p <= 0)
      Map.empty
    else
      Util.makeMapUnsafe(
        (1 to math.min(amount.p, config.winningAmount.p - amount.p)).map(Amount(_))
      ) { move =>
        config.headsDistribution.map { winningBet =>
          val newAmount = if (winningBet) move.p + amount.p else move.p - amount.p
          val reward =
            if (newAmount == config.winningAmount.p)
              config.winningReward
            else
              0
          (reward, copy(amount = Amount(newAmount)))
        }
      }
}
