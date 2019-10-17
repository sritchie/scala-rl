/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy
package bandit

import cats.Monad
import com.twitter.algebird.{AveragedValue, Semigroup}
import io.samritchie.rl.util.ToDouble
import io.samritchie.rl.value.ActionValueMap
import Util.Instances._

/**
  * This is a version that accumulates the reward using a monoid.
  *
  * @param epsilon number between 0 and 1.
  */
case class Greedy[Obs, A, R, S[_]](
    config: Greedy.Config[R, _],
    valueFn: ActionValueFn[Obs, A, R]
) extends CategoricalPolicy[Obs, A, R, S] {
  private val explore: Cat[Boolean] =
    Cat.boolean(config.epsilon)

  private def allActions(state: State[Obs, A, R, S]): Cat[A] =
    Cat.fromSet(state.actions)

  private def greedy(state: State[Obs, A, R, S]): Cat[A] = {
    val obs = state.observation
    Cat.fromSet(
      Util.allMaxBy(state.actions)(
        // TODO deal with initial values?
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
  ): Greedy[Obs, A, R, S] =
    copy(valueFn = valueFn.learn(state.observation, action, reward))
}

// Oh boy, this really does look like it needs an aggregator... maybe
// I build it without, but then include the algebird versions
// elsewhere? Or maybe I build to the cats interfaces, then I have an
// algebird package? More for later.
object Greedy {
  // TODO - restructure this, and UCB and gradient, so they just take an action
  // value function. Then they become solid policies that I can use.
  //
  // I DON'T THINK I need to worry about the ToDouble thing. I just need
  // rankings, right?? Is that all that's important for a policy?
  case class Config[R, T: Semigroup: Ordering: ToDouble](
      epsilon: Double,
      prepare: R => T,
      initial: T
  ) {
    def policy[A, Obs, S[_]]: Greedy[Obs, A, R, S] =
      Greedy(this, ActionValueMap.empty(prepare, initial))
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
