/**
  * First crack at a policy that is actually greedy with respect to some action
  * value function.

  SOOOO to work this actually needs some insight into what comes next.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical
import Util.Instances._

/**
  * My first crack at a proper greedy policy, given some state value. This is
  * the more extensive thing, and of course I'll need to swap it in for the
  * EpsilonGreedy stuff too.

  This policy will be greedy with respect to the value function it's using. The
  state value function. It can only do this because it can see the results of
  what comes next.

  I think you can only be greedy with respect to a state-value function if you
  have access to a model, and can look ahead.
  */
case class Greedy[A, Obs](
    stateValue: StateValue[Obs],
    epsilon: Double
) extends CategoricalPolicy[A, Obs, Real, Id] {

  override def choose(state: State[A, Obs, Real, Id]): Categorical[A] = {
    val candidates = Util.allMaxBy[A, Real](state.actions) { a =>
      state.act(a) match {
        case None => Real.negInfinity
        case Some(Id((r, newS))) =>
          r + (stateValue.m.getOrElse(newS.observation, Real.zero) * epsilon)
      }
    }
    Categorical.fromSet(
      if (candidates.isEmpty) state.actions else candidates
    )
  }
}
