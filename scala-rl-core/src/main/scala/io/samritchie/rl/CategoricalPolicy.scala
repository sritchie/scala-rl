package io.samritchie.rl

import com.stripe.rainier.core.{Categorical, Generator}

/**
  * Policy based on a discrete number of actions. This is a policy
  * where you can DIRECTLY UNDERSTAND what it's trying to do!
  */
trait CategoricalPolicy[A, -Obs, R, S[_]] extends Policy[A, Obs, R, Generator, S] {
  def categories(state: State[A, Obs, R, S]): Categorical[A]

  override def choose(state: State[A, Obs, R, S]): Generator[A] =
    categories(state).generator
}
