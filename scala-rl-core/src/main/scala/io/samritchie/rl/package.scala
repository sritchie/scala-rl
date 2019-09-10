package io.samritchie

import cats.Eval
import com.stripe.rainier.core.Generator

package object rl {
  type State[A, +Obs, +Reward] = BaseState[A, Obs, Reward, Generator]
  type NowState[A, +Obs, +Reward] = BaseState[A, Obs, Reward, Eval]

  type Policy[A, -Obs, -R, This <: Policy[A, Obs, R, This]] =
    BasePolicy[A, Obs, R, Generator, Generator, This]

  type NowPolicy[A, -Obs, -R, This <: NowPolicy[A, Obs, R, This]] =
    SoloPolicy[A, Obs, R, Eval, This]
}
