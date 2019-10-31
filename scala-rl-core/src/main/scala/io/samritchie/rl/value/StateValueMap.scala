package io.samritchie.rl
package value

case class StateValueMap[Obs, T](
    m: Map[Obs, T],
    default: T
) extends StateValueFn[Obs, T] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): T = m.getOrElse(obs, default)

  override def update(observation: Obs, value: T): StateValueFn[Obs, T] =
    copy(m = m.updated(observation, value))
}
