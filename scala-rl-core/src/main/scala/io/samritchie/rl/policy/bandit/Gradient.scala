/**
  * Policy that accumulates using the Gradient.
  */
package io.samritchie.rl
package policy
package bandit

import com.twitter.algebird.{Aggregator, AveragedValue}
import io.samritchie.rl.util.ToDouble

/**
  * This thing needs to track its average reward internally... then,
  * if we have the gradient baseline set, use that thing to generate
  * the notes.
  *
  * T is the "average" type.
  *
  */
case class Gradient[A: Equiv, R: ToDouble, T: ToDouble, S[_]](
    config: Gradient.Config[R, T],
    actionValues: Map[A, Gradient.Item[T]]
) extends CategoricalPolicy[Any, A, R, S] {
  /**
    * Let's try out this style for a bit. This gives us a way to
    * convert an action directly into a probability, using our
    * actionValue Map above.
    */
  implicit val aToDouble: ToDouble[A] =
    Gradient.Item
      .itemToDouble[T]
      .contramap[A](
        actionValues.getOrElse(_, config.initial)
      )

  override def choose(state: State[Any, A, R, S]): Cat[A] =
    Cat.softmax(state.actions)

  override def learn(state: State[Any, A, R, S], action: A, reward: R): Gradient[A, R, T, S] = {
    val pmf = Cat.softmax(state.actions).pmf

    val updated = state.actions.foldLeft(Map.empty[A, Gradient.Item[T]]) {
      case (m, a) =>
        val old = actionValues.getOrElse(a, config.initial)
        val newV =
          if (Equiv[A].equiv(a, action))
            config.combine(old, reward, pmf(a))
          else
            config.combine(old, reward, 1 - pmf(a))
        m.updated(a, newV)
    }
    copy(actionValues = updated)
  }
}

object Gradient {
  import Util.Instances.avToDouble

  object Item {
    implicit def itemToDouble[T]: ToDouble[Item[T]] =
      ToDouble.instance(_.q)
  }

  /**
    * Represents an action value AND some sort of accumulated value.
    */
  case class Item[T](q: Double, t: T)

  /**
    * Holds properties necessary to run the gradient algorithm.
    */
  case class Config[R: ToDouble, T: ToDouble](
      initial: Item[T],
      stepSize: Double,
      prepare: R => T,
      plus: (T, T) => T
  ) {
    /**
      * Generates an actual policy from the supplied config.
      */
    def policy[A, S[_]]: Gradient[A, R, T, S] = Gradient(this, Map.empty[A, Item[T]])

    /**
      * This performs the gradient update step.
      */
    private[rl] def combine(item: Item[T], reward: R, actionProb: Double): Item[T] =
      Gradient.Item(
        item.q + (stepSize * (ToDouble[R].apply(reward) - ToDouble[T].apply(item.t)) * actionProb),
        plus(item.t, prepare(reward))
      )
  }

  /**
    * Hand-selected version that uses AveragedValue to accumulate
    * internally.
    */
  def incrementalConfig(stepSize: Double, initial: Double = 0.0): Config[Double, AveragedValue] =
    Config(Item(0.0, AveragedValue(initial)), stepSize, AveragedValue(_), _ + _)

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
      Item(0.0, initial),
      stepSize,
      agg.prepare(_),
      agg.semigroup.plus(_, _)
    )
  }
}
