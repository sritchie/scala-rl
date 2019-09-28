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

import cats.Id
import com.stripe.rainier.compute.{Real, ToReal}
import com.stripe.rainier.core.Categorical
import Util.Instances._

/**
  * My first crack at a proper greedy policy, given some state value. This is
  * the more extensive thing, and of course I'll need to swap it in for the
  * EpsilonGreedy stuff too.
  *
  * This policy will be greedy with respect to the value function it's using.
  * The state value function. It can only do this because it can see the results
  * of what comes next.
  *
  * I think you can only be greedy with respect to a state-value function if you
  * have access to a model, and can look ahead.
  */
case class Greedy[A, Obs, R: ToReal](valueFn: ValueFunction[Obs]) extends CategoricalPolicy[A, Obs, R, Id] {
  override def choose(state: State[A, Obs, R, Id]): Categorical[A] = {
    val candidates = Util.allMaxBy[A, Real](state.actions) { a =>
      state.act(a) match {
        // This should NOT happen, since we're getting our list directly from
        // the state.
        case None => Real.negInfinity
        case Some((r, newS)) =>
          valueFn.stateValue(newS.observation).from(ToReal(r)).get
      }
    }
    Categorical.fromSet(
      if (candidates.isEmpty) state.actions else candidates
    )
  }

  override def learnAll[O2 <: Obs](vf: ValueFunction[O2]): Policy[A, O2, R, Categorical, Id] = Greedy(vf)
}
