/**
  This is a contramapped value function.
  */
package io.samritchie.rl
package value

import cats.arrow.FunctionK
import com.stripe.rainier.compute.{Real, ToReal}

class Contramapped[Obs, M[_], N[_], S[_]](
    self: ValueFunction[Obs, M, S],
    f: FunctionK[N, M]
) extends ValueFunction[Obs, N, S] {
  def seen: Iterable[Obs] = self.seen
  def stateValue(obs: Obs): Value[Real] = self.stateValue(obs)

  def evaluate[A, R: ToReal](state: State[A, Obs, R, S], policy: Policy[A, Obs, R, N, S]): Value[Real] =
    self.evaluate(state, policy.mapK(f))

  def update[A, R: ToReal](state: State[A, Obs, R, S], value: Value[Real]): ValueFunction[Obs, N, S] =
    new Contramapped(self.update(state, value), f)

  override def evaluateAndUpdate[A, R: ToReal](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, N, S]
  ): ValueFunction[Obs, N, S] =
    new Contramapped(self.evaluateAndUpdate(state, policy.mapK(f)), f)
}

class ContramappedAV[A, Obs, M[_], N[_], S[_]](
    self: ActionValueFunction[A, Obs, M, S],
    f: FunctionK[N, M]
) extends Contramapped[Obs, M, N, S](self, f)
    with ActionValueFunction[A, Obs, N, S] {
  def seen(obs: Obs): Set[A] = self.seen(obs)
  def actionValue(obs: Obs, a: A): Value[Real] = self.actionValue(obs, a)
  def learn[R](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, N, S],
      action: A,
      reward: R
  ): ActionValueFunction[A, Obs, N, S] =
    new ContramappedAV(self.learn(state, policy.mapK(f), action, reward), f)
}
