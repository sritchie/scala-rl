/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy
package bandit

import cats.Monad
import com.twitter.algebird.{AveragedValue, Monoid, Semigroup}
import io.samritchie.rl.value.ActionValueMap
import Util.Instances._

/**
  * This is a version that accumulates the reward using a monoid.
  *
  * @param epsilon number between 0 and 1.
  */
case class Greedy[Obs, A, R, T: Ordering, S[_]](
    config: Greedy.Config[R, T],
    valueFn: ActionValueFn[Obs, A, T]
) extends Policy[Obs, A, R, Cat, S] {
  private val explore: Cat[Boolean] =
    Cat.boolean(config.epsilon)

  private def allActions(state: State[Obs, A, R, S]): Cat[A] =
    Cat.fromSet(state.actions)

  private def greedy(state: State[Obs, A, R, S]): Cat[A] = {
    val obs = state.observation
    Cat.fromSet(
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
  override def learn(
      state: State[Obs, A, R, S],
      action: A,
      reward: R
  ): Greedy[Obs, A, R, T, S] =
    copy(
      valueFn = valueFn.learn(
        state.observation,
        action,
        config.prepare(reward)
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
      Greedy(this, ActionValueMap.empty(Monoid.from(initial)(Semigroup.plus(_, _))))
  }

  /**
    * Returns an incremental config.
    *
    * TODO we also need a version that uses a constant step size,
    * instead of sample averages. And maybe a version that uses
    * exponential decay?
    *
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
