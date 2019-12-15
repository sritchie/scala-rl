/**
  * Policy that accumulates using the Gradient.
  */
package io.samritchie.rl
package policy
package bandit

import com.twitter.algebird.{Aggregator, AveragedValue, Monoid, Semigroup}
import io.samritchie.rl.algebra.ToDouble
import io.samritchie.rl.rainier.Categorical

/**
  * This thing needs to track its average reward internally... then,
  * if we have the gradient baseline set, use that thing to generate
  * the notes.
  *
  * T is the "average" type.
  *
  */
case class Gradient[Obs, A: Equiv, R: ToDouble, T: ToDouble, S[_]](
    config: Gradient.Config[R, T],
    valueFn: ActionValueFn[Obs, A, Gradient.Item[T]]
) extends Policy[Obs, A, R, Cat, S] {

  /**
    * Let's try out this style for a bit. This gives us a way to
    * convert an action directly into a probability, using our
    * actionValue Map above.


    */
  def aToDouble(obs: Obs): ToDouble[A] =
    Gradient.Item
      .itemToDouble[T]
      .contramap[A](
        valueFn.actionValue(obs, _)
      )

  override def choose(state: State[Obs, A, R, S]): Cat[A] = {
    implicit val at = aToDouble(state.observation)
    Categorical.softmax(state.actions)
  }

  override def learn(sars: SARS[Obs, A, R, S]): This = {
    val SARS(state, action, reward, nextState) = sars

    val pmf = choose(state).pmf
    val obs = state.observation

    val updated = state.actions.foldLeft(valueFn) {
      case (vfn, a) =>
        // the new item has to get bootstrapped with the old value... that is
        // SORT of associative, and works. Test soon.
        val old = valueFn.actionValue(state.observation, a).t

        // get the delta,
        val delta = ToDouble[R].apply(reward) - ToDouble[T].apply(old)

        // then there might be some nicer way of doing this.
        val actionProb =
          if (Equiv[A].equiv(a, action))
            -pmf(a)
          else
            1 - pmf(a)

        // this is definitely the baseline.
        val newItem = Gradient.Item(
          actionProb * delta * config.stepSize,
          config.prepare(reward)
        )
        vfn.update(obs, a, newItem)
    }
    copy(valueFn = updated)
  }
}

object Gradient {
  import Util.Instances.avToDouble

  object Item {
    class ItemSemigroup[T](implicit T: Semigroup[T]) extends Semigroup[Item[T]] {
      override def plus(l: Item[T], r: Item[T]): Item[T] =
        Item(l.q + r.q, T.plus(l.t, r.t))
    }

    // Monoid instance, not used for now but meaningful, I think.
    class ItemMonoid[T](implicit T: Monoid[T]) extends ItemSemigroup[T] with Monoid[Item[T]] {
      override val zero: Item[T] = Item(0, T.zero)
    }

    // implicit instances.
    implicit def semigroup[T: Semigroup]: Semigroup[Item[T]] = new ItemSemigroup[T]
    implicit def ord[T: Ordering]: Ordering[Item[T]] = Ordering.by(_.t)
    implicit def monoid[T: Monoid] = new ItemMonoid[T]
    implicit def itemToDouble[T]: ToDouble[Item[T]] = ToDouble.instance(_.q)
  }

  /**
    * Represents an action value AND some sort of accumulated value. The action
    * value is something we get by aggregating a reward in some way.

    You might just sum, which would be goofy; you might do some averaged value,
    or exponentially decaying average.

    The t is the reward aggregator. The q is the item that's getting updated in
    this funky way.

    So how would you write a semigroup for this? You'd have to semigroup combine
    the T... what is the monoid on the q?
    */
  case class Item[T](q: Double, t: T)

  /**
    * Holds properties necessary to run the gradient algorithm.
    */
  case class Config[R: ToDouble, T: ToDouble](
      initial: T,
      stepSize: Double,
      prepare: R => T,
      plus: (T, T) => T
  ) {
    implicit val m: Monoid[T] = Monoid.from(initial)(plus)

    /**
      * Generates an actual policy from the supplied config.
      */
    def policy[Obs, A, S[_]]: Gradient[Obs, A, R, T, S] =
      Gradient(this, ActionValueFn.mergeable[Obs, A, Item[T]])
  }

  /**
    * Hand-selected version that uses AveragedValue to accumulate
    * internally.
    */
  def incrementalConfig(stepSize: Double, initial: Double = 0.0): Config[Double, AveragedValue] =
    Config(AveragedValue(initial), stepSize, AveragedValue(_), _ + _)

  /**
    * Uses NO averaging baseline.
    */
  def noBaseline(stepSize: Double): Config[Double, Unit] =
    fromAggregator(stepSize, (), Aggregator.const(0.0))

  /**
    * Generate this gradient from some aggregator.
    */
  def fromAggregator[R: ToDouble, T](
      stepSize: Double,
      initial: T,
      agg: Aggregator[R, T, Double]
  ): Config[R, T] = {
    implicit val tToDouble: ToDouble[T] = ToDouble.instance(agg.present(_))
    Config(
      initial,
      stepSize,
      agg.prepare(_),
      agg.semigroup.plus(_, _)
    )
  }
}
