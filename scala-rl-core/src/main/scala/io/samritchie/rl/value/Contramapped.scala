/**
  This is a contramapped value function.
  */
package io.samritchie.rl
package value

import cats.arrow.FunctionK
import io.samritchie.rl.util.ToDouble

class Contramapped[Obs, M[_], N[_], S[_]](
    self: ValueFunction[Obs, M, S],
    policyFn: FunctionK[N, M]
) extends ValueFunction[Obs, N, S] {
  override def seen: Iterable[Obs] = self.seen
  override def stateValue(obs: Obs): Value[Double] = self.stateValue(obs)

  override def evaluate[A, R: ToDouble](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, N, S]
  ): Value[Double] =
    self.evaluate(state, policy.mapK(policyFn))

  override def update(obs: Obs, value: Value[Double]): ValueFunction[Obs, N, S] =
    new Contramapped(self.update(obs, value), policyFn)

  override def evaluateAndUpdate[A, R: ToDouble](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, N, S]
  ): ValueFunction[Obs, N, S] =
    new Contramapped(self.evaluateAndUpdate(state, policy.mapK(policyFn)), policyFn)
}

class ContramappedAV[A, Obs, M[_], N[_], S[_]](
    self: ActionValueFunction[A, Obs, M, S],
    policyFn: FunctionK[N, M]
) extends Contramapped[Obs, M, N, S](self, policyFn)
    with ActionValueFunction[A, Obs, N, S] {
  def seen(obs: Obs): Set[A] = self.seen(obs)
  def actionValue(obs: Obs, a: A): Value[Double] = self.actionValue(obs, a)
  def learn[R](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, N, S],
      action: A,
      reward: R
  ): ActionValueFunction[A, Obs, N, S] =
    new ContramappedAV[A, Obs, M, N, S](
      self.learn(state, policy.mapK(policyFn), action, reward),
      policyFn
    )
}
