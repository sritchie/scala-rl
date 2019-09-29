/**
  First crack at a policy that is actually greedy with respect to some action
  value function.

  SOOOO to work this actually needs some insight into what comes next.

  TODO Eval is actually a nice interface for only being able to look ahead so
  far. If it's a Now, you can look directly in. But then you can't look further.
  That'll come in handy later when we try to make games, etc.
  */
package io.samritchie.rl
package policy

import cats.{Id, Monad}
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.ToReal
import com.stripe.rainier.core.Categorical

/**
  * My first crack at a proper epsilon-greedy policy, given some state value.
  * This is the more extensive thing.
  *
  * This policy will be greedy with respect to the value function it's using.
  * The state value function. It can only do this because it can see the results
  * of what comes next.
  *
  * I think you can only be greedy with respect to a state-value function if you
  * have access to a model, and can look ahead.
  */
abstract class AbstractGreedy[A, Obs, R: ToReal, S[_]](
    config: Greedy.Config[R],
    valueFn: ValueFunction[Obs, Categorical, S]
) extends CategoricalPolicy[A, Obs, R, S] {
  // Extend these.
  def greedyOptions(state: State[A, Obs, R, S]): Set[A]

  def learnAll[O2 <: Obs](
      vf: ValueFunction[O2, Categorical, S]
  ): Policy[A, O2, R, Categorical, S]

  private def greedy(state: State[A, Obs, R, S]): Categorical[A] =
    Categorical.fromSet {
      val candidates = greedyOptions(state)
      if (candidates.isEmpty) state.actions else candidates
    }

  private val explore: Categorical[Boolean] =
    Categorical.boolean(config.epsilon)

  private def allActions(state: State[A, Obs, R, S]): Categorical[A] =
    Categorical.list(state.actions.toList)

  override def choose(state: State[A, Obs, R, S]): Categorical[A] =
    Monad[Categorical].ifM(explore)(allActions(state), greedy(state))

}

class Greedy[A, Obs, R: ToReal](
    config: Greedy.Config[R],
    valueFn: ValueFunction[Obs, Categorical, Id]
) extends AbstractGreedy[A, Obs, R, Id](config, valueFn) {
  def greedyOptions(state: State[A, Obs, R, Id]): Set[A] =
    ValueFunction.greedyOptions(valueFn, state)

  override def learnAll[O2 <: Obs](
      vf: ValueFunction[O2, Categorical, Id]
  ): Policy[A, O2, R, Categorical, Id] = new Greedy(config, vf)
}

class StochasticGreedy[A, Obs, R: ToReal](
    config: Greedy.Config[R],
    valueFn: ValueFunction[Obs, Categorical, Categorical]
) extends AbstractGreedy[A, Obs, R, Categorical](config, valueFn) {
  def greedyOptions(state: State[A, Obs, R, Categorical]): Set[A] =
    ValueFunction.greedyOptionsStochastic(valueFn, state)

  override def learnAll[O2 <: Obs](
      vf: ValueFunction[O2, Categorical, Categorical]
  ): Policy[A, O2, R, Categorical, Categorical] = new StochasticGreedy(config, vf)
}

object Greedy {
  case class Config[R](epsilon: Double) {
    def policy[A, Obs](
        valueFn: ValueFunction[Obs, Categorical, Id]
    )(implicit tr: ToReal[R]): Greedy[A, Obs, R] =
      new Greedy(this, valueFn)

    def stochastic[A, Obs](
        valueFn: ValueFunction[Obs, Categorical, Categorical]
    )(implicit tr: ToReal[R]): StochasticGreedy[A, Obs, R] =
      new StochasticGreedy(this, valueFn)
  }
}
