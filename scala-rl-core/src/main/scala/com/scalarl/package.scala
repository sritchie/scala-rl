package com

import cats.Id
import com.stripe.rainier.core.Generator

package object scalarl {

  /**
    I'm leaving these here for now, since I think they WILL be helpful once
    everything settles down... but for now, let's avoid using these.
    */
  type StochasticState[Obs, A, Reward] = State[Obs, A, Reward, Generator]
  type NowState[Obs, A, Reward] = State[Obs, A, Reward, Id]

  type Cat[+T] = rainier.Categorical[T]
}
