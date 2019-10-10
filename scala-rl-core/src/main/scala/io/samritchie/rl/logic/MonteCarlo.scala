/**
  Logic for playing episodic games.
  */
package io.samritchie.rl
package logic

import cats.Monad
import cats.implicits._
import com.twitter.algebird.{Aggregator, Monoid, MonoidAggregator}
import io.samritchie.rl.util.FrequencyTracker

object MonteCarlo {
  type Tracker[Obs, A, R, T] = MonoidAggregator[(Obs, A, R), T, Iterator[((Obs, A, R), Boolean)]]

  object Tracker {
    type FirstVisit[Obs, A, R] = Tracker[Obs, A, R, FrequencyTracker[(Obs, A, R), Obs]]
    type EveryVisit[Obs, A, R] = Tracker[Obs, A, R, Vector[(Obs, A, R)]]

    def firstVisit[Obs, A, R]: FirstVisit[Obs, A, R] = {
      implicit val m = FrequencyTracker.monoid[(Obs, A, R), Obs](_._1)
      Aggregator
        .appendMonoid[(Obs, A, R), FrequencyTracker[(Obs, A, R), Obs], Iterator[((Obs, A, R), Boolean)]](
          _ :+ _,
          _.reverseIterator.map { case (t, seen) => (t, seen == 0) }
        )
    }

    def everyVisit[Obs, A, R]: EveryVisit[Obs, A, R] =
      new EveryVisit[Obs, A, R] {
        def prepare(input: (Obs, A, R)) = Vector(input)
        val monoid = implicitly[Monoid[Vector[(Obs, A, R)]]]
        override def present(t: Vector[(Obs, A, R)]) =
          t.reverseIterator.map((_, true))
      }
  }

  def playTurn[A, Obs, R, M[_]: Monad](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      penalty: R
  ): M[(state.This, (Obs, A, R))] =
    policy.choose(state).flatMap { a =>
      state
        .act(a)
        .getOrElse((penalty, state).pure[M])
        .map { case (r, s2) => (s2, (state.observation, a, r)) }
    }

  /**
    Takes a static policy and a starting state, and a penalty for moves that
    aren't allowed, and returns a context containing the final state and the
    trajectory that got us there.
    */
  def playEpisode[A, Obs, R, M[_]: Monad, T](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      tracker: Tracker[Obs, A, R, T],
      penalty: R
  ): M[(state.This, Iterator[((Obs, A, R), Boolean)])] =
    Util.iterateUntilM(state, tracker)(
      playTurn(policy, _, penalty)
    )(_.isTerminal)

  /**
    Specialized version that keeps track of frequencies too.
    */
  def firstVisit[A, Obs, R, M[_]: Monad](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      penalty: R
  ): M[(state.This, Iterator[((Obs, A, R), Boolean)])] =
    playEpisode[A, Obs, R, M, FrequencyTracker[(Obs, A, R), Obs]](
      policy,
      state,
      Tracker.firstVisit,
      penalty
    )

  def processFirstVisit[Obs, A, R](
      firstVisit: FrequencyTracker[(Obs, A, R), Obs],
      valueFn: ActionValueFunction[Obs, A, R],
      zero: Value[R]
  ): ActionValueFunction[Obs, A, R] =
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    firstVisit.reverseIterator
      .foldLeft((valueFn, zero)) {
        case ((vf, acc), ((obs, a, r), seenCount)) =>
          val newAcc = acc.from(r)
          if (seenCount == 0) {
            (vf.learn(obs, a, newAcc.get), newAcc)
          } else (vf, newAcc)
      }
      ._1
}
