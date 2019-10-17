package io.samritchie.rl
package value

case class StateValueMap[Obs](
    m: Map[Obs, Value[Double]],
    default: Value[Double]
) extends StateValueFn[Obs] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): Value[Double] =
    m.getOrElse(obs, default)

  override def update(observation: Obs, value: Value[Double]): StateValueFn[Obs] =
    copy(m = m.updated(observation, value))
}
