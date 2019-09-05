/**
  * Policy that accumulates using the UCB algo.
  */
package io.samritchie.rl
package policy

import com.twitter.algebird.Semigroup
import com.stripe.rainier.core.Generator

case class UCB[A, R, T: Ordering](
    config: UCB.Config[R, T],
    actionValues: Map[A, UCB.Choice[T]],
    time: Time
) extends Policy[A, R, UCB[A, R, T]] {

  override def choose(state: State[A, R]): Generator[A] =
    Util.generatorFromSet(
      Util.allMaxBy(state.actions)(actionValues(_).totalValue(time))
    )

  override def learn(state: State[A, R], action: A, reward: R): UCB[A, R, T] = {
    val updated = Util.updateWith(actionValues, action) {
      case None    => config.choice(reward)
      case Some(v) => config.merge(v, reward)
    }
    copy(actionValues = updated, time = time.tick)
  }
}

object UCB {
  case class Config[R, T: Ordering: Semigroup](
      param: Param,
      prepare: R => T,
      fromDouble: Double => T
  ) {
    def policy[A]: UCB[A, R, T] = UCB(this, Map.empty, Time.zero)

    private[rl] def merge(choice: Choice[T], r: R) = choice + prepare(r)
    private[rl] def choice(r: R): Choice[T] =
      UCB.Choice.one(prepare(r), param)(fromDouble)
  }

  case class Param(c: Int) extends AnyVal

  object Choice {
    def one[A: Ordering: Semigroup](a: A, param: Param)(fromDouble: Double => A): Choice[A] =
      Choice(a, 1L, param, fromDouble)
  }

  case class Choice[T: Ordering: Semigroup](t: T, visits: Long, param: Param, fromDouble: Double => T) {
    def +(delta: T): Choice[T] =
      copy(Semigroup.plus[T](t, delta), visits + 1, param, fromDouble)

    def totalValue(time: Time): T =
      if (visits <= 0) t
      else Semigroup.plus(t, bonus(time))

    def compare(other: Choice[T], time: Time): Int = (this.visits, other.visits) match {
      case (0L, 0L) => 0
      case (0L, _)  => 1
      case (_, 0L)  => -1
      case _ =>
        Ordering[T].compare(
          totalValue(time),
          other.totalValue(time)
        )
    }

    // Only called if visits is > 0.
    private def bonus(time: Time): T =
      fromDouble(param.c * math.sqrt(math.log(time.value + 1)) / visits)
  }
}
