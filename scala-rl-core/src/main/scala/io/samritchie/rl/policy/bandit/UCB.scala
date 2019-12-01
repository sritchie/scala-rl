/**
  * Policy that accumulates using the UCB algo.
  *
  * TODO should I make an Empty Choice option with a sealed trait?
  */
package io.samritchie.rl
package policy
package bandit

import com.twitter.algebird.Aggregator

case class UCB[A, R, T, S[_]](
    config: UCB.Config[R, T],
    actionValues: Map[A, UCB.Choice[T]],
    time: Time
) extends CategoricalPolicy[Any, A, R, S] {
  override def choose(state: State[Any, A, R, S]): Cat[A] =
    Cat.fromSet(
      Util
        .allMaxBy(state.actions)(
          a => actionValues.getOrElse(a, config.initialChoice).totalValue(time)
        )
    )

  override def learn(
      state: State[Any, A, R, S],
      action: A,
      reward: R
  ): UCB[A, R, T, S] = {
    val updated = Util.updateWith(actionValues, action) {
      case None    => config.choice(reward)
      case Some(v) => config.merge(v, reward)
    }
    copy(actionValues = updated, time = time.tick)
  }
}

object UCB {
  /**
    * Generates a Config instance from an algebird Aggregator and a
    * UCB parameter.
    */
  def fromAggregator[R, T: Ordering](
      initial: T,
      param: Param,
      agg: Aggregator[R, T, Double]
  ): Config[R, T] =
    Config(param, initial, agg.prepare _, agg.semigroup.plus _, agg.present _)

  case class Config[R, T: Ordering](
      param: Param,
      initial: T,
      prepare: R => T,
      plus: (T, T) => T,
      present: T => Double
  ) {
    /**
      * Returns a fresh policy instance using this config.
      */
    def policy[A, S[_]]: UCB[A, R, T, S] = UCB(this, Map.empty, Time.Zero)

    // These are private and embedded in the config to make it easy to
    // share the fns without crossing the beams.
    private[rl] def merge(choice: Choice[T], r: R) = choice.update(plus(_, prepare(r)))
    private[rl] def choice(r: R): Choice[T] =
      UCB.Choice.one(prepare(r), param)(present)

    def initialChoice: Choice[T] = UCB.Choice.zero(initial, param)(present)
  }

  /**
    * Tunes how important the upper confidence bound business is.
    */
  case class Param(c: Int) extends AnyVal

  object Choice {
    def zero[T: Ordering](initial: T, param: Param)(toDouble: T => Double): Choice[T] =
      Choice(initial, 1L, param, toDouble)

    def one[T: Ordering](t: T, param: Param)(toDouble: T => Double): Choice[T] =
      Choice(t, 1L, param, toDouble)
  }

  /**
    * Tracks the info required for the UCB calculation.
    */
  case class Choice[T](
      t: T,
      visits: Long,
      param: Param,
      toDouble: T => Double
  ) {
    /**
      * Updates the contained value, increments the visits.
      */
    def update(f: T => T): Choice[T] =
      copy(f(t), visits + 1, param, toDouble)

    def totalValue(time: Time): Double =
      if (visits <= 0) toDouble(t)
      else toDouble(t) + bonus(time)

    def compare(other: Choice[T], time: Time): Int = (this.visits, other.visits) match {
      case (0L, 0L) => 0
      case (0L, _)  => 1
      case (_, 0L)  => -1
      case _ =>
        Ordering[Double].compare(
          totalValue(time),
          other.totalValue(time)
        )
    }

    // Only called if visits is > 0.
    private def bonus(time: Time): Double =
      param.c * math.sqrt(math.log(time.value + 1)) / visits
  }
}
