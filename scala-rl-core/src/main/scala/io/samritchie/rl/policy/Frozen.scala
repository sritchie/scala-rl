package io.samritchie.rl
package policy

/**
  Wrapper for a policy that prevents it from learning.
  */
case class LearningGate[T, A, Obs, R, M[_], S[_]](
    policy: Policy[A, Obs, R, M, S],
    gate: T,
    gateFn: T => Either[T, T]
) extends Policy[A, Obs, R, M, S] { self =>
  def choose(state: State[A, Obs, R, S]): M[A] = policy.choose(state)
  override def learn(state: State[A, Obs, R, S], action: A, reward: R): Policy[A, Obs, R, M, S] =
    gateFn(gate) match {
      case Left(t)  => LearningGate(policy, t, gateFn)
      case Right(t) => LearningGate(policy.learn(state, action, reward), t, gateFn)
    }
}
