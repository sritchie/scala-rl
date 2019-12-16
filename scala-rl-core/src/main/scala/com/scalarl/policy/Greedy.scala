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
package com.scalarl
package policy

import cats.{Id, Monad}
import com.scalarl.algebra.{Expectation, Module}
import com.scalarl.evaluate.ActionValue
import com.scalarl.rainier.Categorical

/**
Base logic for greedy policies.
  */
class Greedy[Obs, A, R, T: Ordering, S[_]](
    evaluator: ActionValue[Obs, A, R, T, S],
    epsilon: Double
) extends Policy[Obs, A, R, Cat, S] { self =>

  private val explore: Cat[Boolean] =
    Categorical.boolean(epsilon)

  private def allActions(state: State[Obs, A, R, S]): Cat[A] =
    Categorical.fromSet(state.actions)

  private def greedy(state: State[Obs, A, R, S]): Cat[A] =
    Categorical.fromSet(evaluator.greedyOptions(state))

  override def choose(state: State[Obs, A, R, S]): Cat[A] =
    Monad[Cat].ifM(explore)(allActions(state), greedy(state))
}

object Greedy {
  import Module.DModule

  case class Config[R, T: DModule: Ordering](
      epsilon: Double,
      prepare: R => T,
      merge: (T, T) => T,
      default: T
  ) {
    def id[Obs, A](valueFn: StateValueFn[Obs, T]): Policy[Obs, A, R, Cat, Id] =
      policy(valueFn)

    def stochastic[Obs, A](valueFn: StateValueFn[Obs, T]): Policy[Obs, A, R, Cat, Cat] =
      policy(valueFn)

    def policy[Obs, A, S[_]: Expectation](valueFn: StateValueFn[Obs, T]): Policy[Obs, A, R, Cat, S] =
      new Greedy[Obs, A, R, T, S](Evaluator.oneAhead(valueFn, prepare, merge), epsilon)
  }
}
