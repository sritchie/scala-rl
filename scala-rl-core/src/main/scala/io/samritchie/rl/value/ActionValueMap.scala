package io.samritchie.rl
package value

import com.twitter.algebird.Semigroup
import io.samritchie.rl.util.ExpectedValue

case class ActionValueMap[Obs, A, T: Semigroup](
    m: Map[Obs, Map[A, T]],
    default: T
) extends ActionValueFn[Obs, A, T] { self =>
  def seenStates: Iterable[Obs] = m.keySet

  override def seen(obs: Obs): Iterable[A] = m.get(obs) match {
    case None    => Seq.empty
    case Some(m) => m.keySet
  }
  override def actionValue(obs: Obs, a: A): T = {
    val tOpt = for {
      at <- m.get(obs)
      t <- at.get(a)
    } yield t
    tOpt.getOrElse(default)
  }

  override def learn(obs: Obs, action: A, value: T): ActionValueMap[Obs, A, T] = {
    val actionM = m.getOrElse(obs, Map.empty[A, T])
    val newM = Util.mergeV(actionM, action, value)
    copy(m = m.updated(obs, newM))
  }

  override def toValueFunction[R, M[_]: ExpectedValue](
      policy: Policy[Obs, A, R, M, Any],
      default: T
  ): StateValueFn[Obs, T] = ???

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
  def empty[Obs, A, T: Semigroup](
      default: T
  ): ActionValueMap[Obs, A, T] =
    ActionValueMap(Map.empty[Obs, Map[A, T]], default)
}
