package io.samritchie

import cats.Eval
import com.stripe.rainier.core.Generator

package object rl {
  type StochasticState[A, +Obs, +Reward] = State[A, Obs, Reward, Generator]
  type NowState[A, +Obs, +Reward] = State[A, Obs, Reward, Eval]
}
