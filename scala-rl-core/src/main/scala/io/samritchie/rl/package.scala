package io.samritchie

import com.stripe.rainier.core.{Categorical, Generator}

package object rl {

  /**
    I'm leaving these here for now, since I think they WILL be helpful once
    everything settles down... but for now, let's avoid using these.
    */
  type StochasticState[A, +Obs, Reward] = State[A, Obs, Reward, Generator]
  type NowState[A, +Obs, Reward] = State[A, Obs, Reward, Id]

  // typeCategoricalPolicy[A, -Obs, R, S[_]] extends Policy[A, Obs, R, Categorical, S]
}
