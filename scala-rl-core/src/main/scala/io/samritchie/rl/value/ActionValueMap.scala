package io.samritchie.rl
package value

import io.samritchie.rl.util.ExpectedValue

case class ActionValueMap[Obs, A, R, T](m: Map[Obs, Map[A, T]], prepare: R => T, present: T => Double)
    extends ActionValueFunction[Obs, A, R] { self =>
  def seenStates: Iterable[Obs] = m.keySet

  def seen(obs: Obs): Iterable[A] = m.get(obs) match {
    case None    => Seq.empty
    case Some(m) => m.keySet
  }
  def actionValue(obs: Obs, a: A): Value[Double] = ???

  def learn(obs: Obs, action: A, value: R): ActionValueFunction[Obs, A, R] = ???

  def update(state: Obs, value: Value[Double]): ValueFunction[Obs] = ???

  def toValueFunction[M[_]: ExpectedValue](
      policy: Policy[Obs, A, R, M, Any],
      default: Value[Double]
  ): ValueFunction[Obs] = ???

  /**
    This is not doing well, now and exposing some of the faults of my older
    approach, and the relationship between action-value and state-value
    functions. This is the good stuff, the most design fun!
    */
  // new ValueFunction[Obs] {
  //   def seen: Iterable[Obs] = self.seenStates
  //   def stateValue(obs: Obs): Value[Double] = {
  //     val aMap = m.getOrElse(obs, Map.empty[A, T])
  //     self.seen(obs).map { action =>
  //       ExpectedValue[M].get(action, default) { a =>
  //         aMap.getOrElse(action, default)
  //       }
  //     }
  //   }
  // }
}
