/**
  First crack at a policy that is actually greedy with respect to some action
  value function.

  Because this only has access to states, to do any updating it needs to be able
  to either look ahead, or to see the dynamics of the system.

  Both of those ideas are implemented below.

  TODO Eval is actually a nice interface for only being able to look ahead so
  far. If it's a Now, you can look directly in. But then you can't look further.
  That'll come in handy later when we try to make games, etc. I can imagine some
  data type that makes it difficult to see, of course. And then your best guess
  has to involve some knowledge of where you might get to, even if you don't
  know the reward.
  */
package io.samritchie.rl
package policy

import cats.{Id, Monad}
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.{Real, ToReal}
import com.stripe.rainier.core.Categorical

/**
Base logic for greedy policies.
  */
abstract class AbstractGreedy[A, Obs, R: ToReal, S[_]](
    config: Greedy.Config[R],
    valueFn: ValueFunction[Obs, Categorical, S]
) extends CategoricalPolicy[A, Obs, R, S] { self =>

  /**
    Returns the set of best actions from the state.
    */
  def greedyOptions(state: State[A, Obs, R, S]): Set[A]

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
}

class StochasticGreedy[A, Obs, R: ToReal](
    config: Greedy.Config[R],
    valueFn: ValueFunction[Obs, Categorical, Categorical]
) extends AbstractGreedy[A, Obs, R, Categorical](config, valueFn) {
  def greedyOptions(state: State[A, Obs, R, Categorical]): Set[A] =
    ValueFunction.greedyOptionsStochastic(valueFn, state, config.default)
}

object Greedy {
  case class Config[R: ToReal](epsilon: Double, default: Value[Real]) {
    def id[A, Obs](
        valueFn: ValueFunction[Obs, Categorical, Id]
    ): CategoricalPolicy[A, Obs, R, Id] =
      new Greedy(this, valueFn)

    def stochastic[A, Obs](
        valueFn: ValueFunction[Obs, Categorical, Categorical]
    ): CategoricalPolicy[A, Obs, R, Categorical] = new StochasticGreedy(this, valueFn)
  }
}
