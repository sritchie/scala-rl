/**
  * First crack at a policy that is actually greedy with respect to some action
  * value function.

  SOOOO to work this actually needs some insight into what comes next.
  */
package io.samritchie.rl
package policy

import cats.Eval

/**
  * My first crack at a proper greedy policy, given some state value. This is
  * the more extensive thing, and of course I'll need to swap it in for the
  * EpsilonGreedy stuff too.

  This policy will be greedy with respect to the value function it's using. The
  state value function. It can only do this because it can see the results of
  what comes next.

  TODO should this be called something like GreedyLookahead?
  */
case class Greedy[A, Obs, R, T](
    stateValue: StateValue[Obs],
    epsilon: Double,
    initial: T
) extends BasePolicy[A, Obs, R, Eval, Eval, Greedy[A, Obs, R, T]] {
  override def choose(state: NowState[A, Obs, R]): Eval[A] = ???
  override def learn(state: NowState[A, Obs, R], action: A, reward: R): Greedy[A, Obs, R, T] = ???
}
