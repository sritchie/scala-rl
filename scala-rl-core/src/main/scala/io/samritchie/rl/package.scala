package io.samritchie

import cats.Id
import com.stripe.rainier.core.Generator

package object rl {

  /**
    I'm leaving these here for now, since I think they WILL be helpful once
    everything settles down... but for now, let's avoid using these.
    */
  type StochasticState[Obs, A, Reward] = State[Obs, A, Reward, Generator]
  type NowState[Obs, A, Reward] = State[Obs, A, Reward, Id]
}
