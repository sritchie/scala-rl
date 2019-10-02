/**
  First crack at a policy that is actually greedy with respect to some action
  value function.

  Because this only has access to states, to do any updating it needs to be able
  to either look ahead, or to see the dynamics of the system.

  Both of those ideas are implemented below.

  TODO Eval is actually a nice interface for only being able to look ahead so
  far. If it's a Now, you can look directly in. But then you can't look further.
  That'll come in handy later when we try to make games, etc. I can imagine some
  data type that makes it difficult to see, of course. And then your best guess
  has to involve some knowledge of where you might get to, even if you don't
  know the reward.
  */
package io.samritchie.rl
package policy

import cats.{Id, Monad}
import io.samritchie.rl.util.{ExpectedValue, ToDouble}

/**
Base logic for greedy policies.
  */
class Greedy[A, Obs, R: ToDouble, M[_], S[_]: ExpectedValue](
    config: Greedy.Config[R],
    valueFn: ValueFunction[Obs, M, S]
) extends Policy[A, Obs, R, Cat, S] { self =>
  private val explore: Cat[Boolean] =
    Cat.boolean(config.epsilon)

  private def allActions(state: State[A, Obs, R, S]): Cat[A] =
    Cat.fromSet(state.actions)

  private def greedy(state: State[A, Obs, R, S]): Cat[A] =
    Cat.fromSet(
      ValueFunction.greedyOptions(valueFn, state, config.default)
    )

  override def choose(state: State[A, Obs, R, S]): Cat[A] =
    Monad[Cat].ifM(explore)(allActions(state), greedy(state))
}

object Greedy {
  case class Config[R: ToDouble](epsilon: Double, default: Value[Double]) {
    def id[A, Obs](
        valueFn: ValueFunction[Obs, Cat, Id]
    ): Policy[A, Obs, R, Cat, Id] =
      new Greedy(this, valueFn)

    def stochastic[A, Obs](
        valueFn: ValueFunction[Obs, Cat, Cat]
    ): Policy[A, Obs, R, Cat, Cat] = new Greedy(this, valueFn)
  }
}
