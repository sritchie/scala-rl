/**
  * Policy that accumulates using the UCB algorithm.
  *
  * TODO should I make an Empty Choice option with a sealed trait?
  */
package com.scalarl
package policy
package bandit

import com.twitter.algebird.{Aggregator, Monoid, Semigroup}
import com.scalarl.evaluate.ActionValue
import com.scalarl.rainier.Categorical

case class UCB[Obs, A, R, T, S[_]](
    config: UCB.Config[R, T],
    valueFn: ActionValueFn[Obs, A, UCB.Choice[T]],
    time: Time
) extends Policy[Obs, A, R, Cat, S] {

  private val evaluator: ActionValue[Obs, A, R, UCB.Choice[T], S] =
    valueFn.toEvaluator

  override def choose(state: State[Obs, A, R, S]): Cat[A] =
    Categorical.fromSet(
      Util
        .allMaxBy(state.actions)(
          evaluator.evaluate(state, _).totalValue(time)
        )
    )

  /**
    learn here passes directly through to the ActionValueFn now, which is the
    new thing. Does this mean that we shouldn't learn at all? Should that get
    delegated to an agent?
    */
  override def learn(sars: SARS[Obs, A, R, S]): This =
    copy(
      valueFn = valueFn.update(
        sars.state.observation,
        sars.action,
        config.choice(sars.reward)
      ),
      time = time.tick
    )
}

object UCB {

  /**
    * Generates a Config instance from an algebird Aggregator and a
    * UCB parameter.
    */
  def fromAggregator[R, T](
      initial: T,
      param: Param,
      agg: Aggregator[R, T, Double]
  ): Config[R, T] =
    Config(param, initial, agg.prepare _, agg.semigroup.plus _, agg.present _)

  case class Config[R, T](
      param: Param,
      initial: T,
      prepare: R => T,
      plus: (T, T) => T,
      present: T => Double
  ) {

    /**
      * Returns a fresh policy instance using this config.
      */
    def policy[Obs, A, S[_]]: UCB[Obs, A, R, T, S] = {
      implicit val tMonoid: Monoid[T] = Monoid.from(initial)(plus)
      implicit val monoid: Monoid[Choice[T]] = Choice.monoid(param, present)
      val avm = ActionValueFn.mergeable[Obs, A, Choice[T]]
      UCB(this, avm, Time.Zero)
    }

    // These are private and embedded in the config to make it easy to
    // share the fns without crossing the beams.
    private[scalarl] def merge(choice: Choice[T], r: R) = choice.update(plus(_, prepare(r)))
    private[scalarl] def choice(r: R): Choice[T] =
      Choice.one(prepare(r), param)(present)

    def initialChoice: Choice[T] = Choice.zero(initial, param)(present)
  }

  /**
    * Tunes how important the upper confidence bound business is.
    */
  case class Param(c: Int) extends AnyVal

  /**
    Needs documentation; this is a way of tracking how many times a particular
    thing was chosen along with its value.
    */
  object Choice {
    // Classes...
    class ChoiceSemigroup[T](implicit T: Semigroup[T]) extends Semigroup[Choice[T]] {
      override def plus(l: Choice[T], r: Choice[T]): Choice[T] =
        l.copy(t = T.plus(l.t, r.t), visits = l.visits + r.visits)
    }

    // Monoid instance, not used for now but meaningful, I think.
    class ChoiceMonoid[T](param: Param, toDouble: T => Double)(implicit T: Monoid[T])
        extends ChoiceSemigroup[T]
        with Monoid[Choice[T]] {
      override val zero: Choice[T] =
        Choice.zero[T](T.zero, param)(toDouble)
    }

    // implicit instances.
    implicit def semigroup[T: Semigroup]: Semigroup[Choice[T]] = new ChoiceSemigroup[T]
    implicit def ord[T: Ordering]: Ordering[Choice[T]] = Ordering.by(_.t)

    def monoid[T: Monoid](param: Param, toDouble: T => Double) = new ChoiceMonoid[T](param, toDouble)

    // constructors.
    def zero[T](initial: T, param: Param)(toDouble: T => Double): Choice[T] =
      Choice(initial, 0L, param, toDouble)

    def one[T](t: T, param: Param)(toDouble: T => Double): Choice[T] =
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
