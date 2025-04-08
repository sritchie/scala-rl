/** Policy that accumulates via epsilon greedy. This is only still here because it knows how to learn.
  */
package com.scalarl
package policy
package bandit

import cats.{Functor, Monad}
import com.twitter.algebird.{AveragedValue, Semigroup}
import com.scalarl.rainier.Categorical
import Util.Instances._

/** @param epsilon
  *   number between 0 and 1.
  */
case class Greedy[Obs, A, R, T: Ordering, S[_]](
    config: Greedy.Config[R, T],
    valueFn: ActionValueFn[Obs, A, T]
) extends Policy[Obs, A, R, Cat, S] {
  implicit val functor: Functor[Cat] = Functor[Cat]

  private val explore: Cat[Boolean] =
    Categorical.boolean(config.epsilon)

  private def allActions(state: State[Obs, A, R, S]): Cat[A] =
    Categorical.fromSet(state.actions)

  private def greedy(state: State[Obs, A, R, S]): Cat[A] = {
    val obs = state.observation
    Categorical.fromSet(
      Util.allMaxBy(state.actions)(
        valueFn.actionValue(obs, _)
      )
    )
  }

  override def choose(state: State[Obs, A, R, S]): Cat[A] =
    Monad[Cat]
      .ifM(explore)(
        allActions(state),
        greedy(state)
      )

  override def learn(sars: SARS[Obs, A, R, S]): This =
    copy(
      valueFn = valueFn.update(
        sars.state.observation,
        sars.action,
        config.prepare(sars.reward)
      )
    )
}

// TODO Oh boy, this really does look like it needs an aggregator... maybe
// I build it without, but then include the algebird versions
// elsewhere? Or maybe I build to the cats interfaces, then I have an
// algebird package? More for later.
object Greedy {
  case class Config[R, T: Semigroup: Ordering](
      epsilon: Double,
      prepare: R => T,
      initial: T
  ) {
    def policy[A, Obs, S[_]]: Greedy[Obs, A, R, T, S] =
      Greedy(this, ActionValueFn.mergeable(initial))
  }

  /** Returns an incremental config.
    *
    * TODO we also need a version that uses a constant step size, instead of sample averages. And maybe a
    * version that uses exponential decay?
    */
  def incrementalConfig(
      epsilon: Double,
      initial: Double = 0.0
  ): Config[Double, AveragedValue] = Config(
    epsilon,
    AveragedValue(_),
    AveragedValue(initial)
  )
}
