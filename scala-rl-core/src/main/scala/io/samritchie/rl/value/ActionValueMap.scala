package io.samritchie.rl
package value

import com.twitter.algebird.Monoid

case class ActionValueMap[Obs, A, T: Monoid](
    m: Map[Obs, Map[A, T]]
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
    tOpt.getOrElse(Monoid.zero[T])
  }

  override def learn(obs: Obs, action: A, value: T): ActionValueMap[Obs, A, T] = {
    val actionM = m.getOrElse(obs, Map.empty[A, T])
    val newM = Util.mergeV(actionM, action, value)
    copy(m = m.updated(obs, newM))
  }
}

object ActionValueMap {
  def empty[Obs, A, T: Monoid]: ActionValueMap[Obs, A, T] =
    ActionValueMap(Map.empty[Obs, Map[A, T]])
}
