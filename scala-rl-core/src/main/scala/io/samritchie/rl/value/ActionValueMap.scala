package io.samritchie.rl
package value

import com.twitter.algebird.Semigroup
import io.samritchie.rl.util.{ExpectedValue, ToDouble}

case class ActionValueMap[Obs, A, R, T: Semigroup: Ordering: ToDouble](
    m: Map[Obs, Map[A, T]],
    prepare: R => T,
    default: T
) extends ActionValueFunction[Obs, A, R] { self =>
  def seenStates: Iterable[Obs] = m.keySet

  override def seen(obs: Obs): Iterable[A] = m.get(obs) match {
    case None    => Seq.empty
    case Some(m) => m.keySet
  }
  override def actionValue(obs: Obs, a: A): Value[Double] = {
    val tOpt = for {
      at <- m.get(obs)
      t <- at.get(a)
    } yield t
    val ret = ToDouble[T].apply(tOpt.getOrElse(default))
    ???
  }

  override def learn(obs: Obs, action: A, value: R): ActionValueMap[Obs, A, R, T] = ???
  // val actionM = actionValues.getOrElse(obs, Map.empty[A, T])
  // val newM = Util.mergeV(actionM, action, config.prepare(reward))

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

object ActionValueMap {
  def empty[Obs, A, R, T: Semigroup: Ordering: ToDouble](
      prepare: R => T,
      default: T
  ): ActionValueMap[Obs, A, R, T] =
    ActionValueMap(Map.empty[Obs, Map[A, T]], prepare, default)
}
