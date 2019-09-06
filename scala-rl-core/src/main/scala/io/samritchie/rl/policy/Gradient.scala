/**
  * Policy that accumulates using the Gradient.
  *
  * TODO figure out how the types can be not so goofed. Do we really
  * want a separate aggregate type?
  *
  * TODO support NO gradient baseline,
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.compute.{Real, ToReal}
import com.stripe.rainier.core.Generator

/**
  * This thing needs to track its average reward internally... then,
  * if we have the gradient baseline set, use that thing to generate
  * the notes.
  *
  * T is the "average" type.
  *
  */
case class Gradient[A: Equiv, R: Numeric, T: Numeric](
    config: Gradient.Config[R, T],
    actionValues: Map[A, Gradient.Item[T]]
) extends Policy[A, R, Gradient[A, R, T]] {

  /**
    * Let's try out this style for a bit. This gives us a way to
    * convert an action directly into a probability, using our
    * actionValue Map above.
    */
  implicit val aToReal: ToReal[A] =
    implicitly[ToReal[Gradient.Item[T]]].contramap(
      actionValues.getOrElse(_, config.initial)
    )

  override def choose(state: State[A, R]): Generator[A] =
    Util.softmax(state.actions).generator

  override def learn(state: State[A, R], action: A, reward: R): Gradient[A, R, T] = {
    val pmf = Util.softmax(state.actions).pmf

    val updated = state.actions.foldLeft(Map.empty[A, Gradient.Item[T]]) {
      case (m, a) =>
        val old = actionValues.getOrElse(a, config.initial)
        val newV =
          if (Equiv[A].equiv(a, action))
            config.combine(old, reward, pmf(a))
          else
            config.combine(old, reward, 1 - probs(a))
        m.updated(a, newV)
    }
    copy(actionValues = updated)
  }
}

object Gradient {
  object Item {
    implicit def toReal[T]: ToReal[Item[T]] =
      ToReal.fromReal.contramap(_.q)
  }

  /**
    * Represents an action value AND an average in progress.
    */
  case class Item[T](q: Real, t: T) {}

  case class Config[R: Numeric, T: Numeric](
      initial: Item[T],
      stepSize: Real,
      prepare: R => T,
      plus: (T, T) => T
  ) {
    def policy[A]: Gradient[A, R, T] = Gradient(this, Map.empty)

    private[rl] def combine(item: Item[T], reward: R, actionProb: Real): Item[T] =
      Gradient.Item(
        item.q + (stepSize * (ToReal(reward) - item.t) * actionProb),
        plus(item.t, prepare(reward))
      )
  }
}
