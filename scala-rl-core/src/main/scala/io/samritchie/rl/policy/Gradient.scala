/**
  * Policy that accumulates using the Gradient.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.compute.{Real, ToReal}
import com.stripe.rainier.core.Categorical
import com.twitter.algebird.{Aggregator, AveragedValue}

/**
  * This thing needs to track its average reward internally... then,
  * if we have the gradient baseline set, use that thing to generate
  * the notes.
  *
  * T is the "average" type.
  *
  */
case class Gradient[A: Equiv, R: ToReal, T: ToReal, S[_]](
    config: Gradient.Config[R, T],
    actionValues: Map[A, Gradient.Item[T]]
) extends CategoricalPolicy[A, Any, R, S] {

  /**
    * Let's try out this style for a bit. This gives us a way to
    * convert an action directly into a probability, using our
    * actionValue Map above.
    */
  implicit val aToReal: ToReal[A] =
    implicitly[ToReal[Gradient.Item[T]]].contramap(
      actionValues.getOrElse(_, config.initial)
    )

  override def choose(state: State[A, Any, R, S]): Categorical[A] =
    Util.softmax(state.actions)

  override def learn(state: State[A, Any, R, S], action: A, reward: R): Gradient[A, R, T, S] = {
    val pmf = Util.softmax(state.actions).pmf

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
  import Util.Instances.avToReal

  object Item {
    implicit def toReal[T]: ToReal[Item[T]] =
      ToReal.fromReal.contramap(_.q)
  }

  /**
    * Represents an action value AND some sort of accumulated value.
    */
  case class Item[T](q: Real, t: T)

  /**
    * Holds properties necessary to run the gradient algorithm.
    */
  case class Config[R: ToReal, T: ToReal](
      initial: Item[T],
      stepSize: Real,
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
    private[rl] def combine(item: Item[T], reward: R, actionProb: Real): Item[T] =
      Gradient.Item(
        item.q + (stepSize * (ToReal(reward) - item.t) * actionProb),
        plus(item.t, prepare(reward))
      )
  }

  /**
    * Hand-selected version that uses AveragedValue to accumulate
    * internally.
    */
  def incrementalConfig(stepSize: Real, initial: Double = 0.0): Config[Double, AveragedValue] =
    Config(Item(Real.zero, AveragedValue(initial)), stepSize, AveragedValue(_), _ + _)

  /**
    * Uses NO averaging baseline.
    */
  def noBaseline(stepSize: Real): Config[Double, Unit] =
    fromAggregator(stepSize, (), Aggregator.const(Real.zero))

  /**
    * Generate this gradient from some aggregator.
    */
  def fromAggregator[R: ToReal, T](stepSize: Real, initial: T, agg: Aggregator[R, T, Real]): Config[R, T] = {
    implicit val toReal: ToReal[T] = ToReal.fromReal.contramap(agg.present(_))
    Config(
      Item(Real.zero, initial),
      stepSize,
      agg.prepare(_),
      agg.semigroup.plus(_, _)
    )
  }
}
