package io.samritchie

import cats.Eval
import com.stripe.rainier.core.Generator

package object rl {
  type State[A, +Obs, +Reward] = BaseState[A, Obs, Reward, Generator]
  type NowState[A, +Obs, +Reward] = BaseState[A, Obs, Reward, Eval]
}
