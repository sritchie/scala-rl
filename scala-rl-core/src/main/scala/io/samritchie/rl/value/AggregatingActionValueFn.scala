package io.samritchie.rl
package value

import com.twitter.algebird.MonoidAggregator

case class AggregatingActionValueFn[Obs, A, T, U](
    m: ActionValueFn[Obs, A, T],
    agg: MonoidAggregator[U, T, U]
) extends ActionValueFn[Obs, A, U] { self =>
  override def seenStates: Iterable[Obs] = m.seenStates

  override def seen(obs: Obs): Iterable[A] = m.seen(obs)

  override def actionValue(obs: Obs, a: A): U =
    agg.present(m.actionValue(obs, a))

  override def learn(obs: Obs, action: A, value: U): AggregatingActionValueFn[Obs, A, T, U] =
    new AggregatingActionValueFn(m.learn(obs, action, agg.prepare(value)), agg)
}
