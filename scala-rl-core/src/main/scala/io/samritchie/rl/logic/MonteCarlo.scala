/**
  Logic for playing episodic games.
  */
package io.samritchie.rl
package logic

import cats.Monad
import cats.implicits._
import com.twitter.algebird.{Aggregator, Monoid, MonoidAggregator}
import io.samritchie.rl.util.FrequencyTracker
import scala.annotation.tailrec

object MonteCarlo {
  case class ShouldUpdateState(get: Boolean) extends AnyVal
  object ShouldUpdateState {
    val yes = ShouldUpdateState(true)
    val no = ShouldUpdateState(false)
  }

  type Snap[Obs, A, R, M[_]] = (State[Obs, A, R, M], A, R)
  type Trajectory[Obs, A, R, M[_]] = Iterator[(Snap[Obs, A, R, M], ShouldUpdateState)]
  type Tracker[Obs, A, R, T, M[_]] = MonoidAggregator[Snap[Obs, A, R, M], T, Trajectory[Obs, A, R, M]]

  object Tracker {
    type FirstVisit[Obs, A, R, M[_]] = Tracker[Obs, A, R, FrequencyTracker[Snap[Obs, A, R, M], Obs], M]
    type EveryVisit[Obs, A, R, M[_]] = Tracker[Obs, A, R, Vector[Snap[Obs, A, R, M]], M]

    def firstVisit[Obs, A, R, M[_]]: FirstVisit[Obs, A, R, M] = {
      implicit val m = FrequencyTracker.monoid[Snap[Obs, A, R, M], Obs](_._1.observation)
      Aggregator.appendMonoid(
        appnd = _ :+ _,
        pres = _.reverseIterator.map { case (t, seen) => (t, ShouldUpdateState(seen == 0)) }
      )
    }

    /**
      Aggregator that returns an iterator of a trajectory, with each state
      paired with a boolean signalling whether or not it should trigger a state
      update.
      */
    def everyVisit[Obs, A, R, M[_]]: EveryVisit[Obs, A, R, M] =
      Aggregator.appendMonoid(
        appnd = _ :+ _,
        pres = _.reverseIterator.map((_, ShouldUpdateState.yes))
      )
  }

  def play[Obs, A, R, M[_]: Monad](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M]
  ): M[(state.This, Snap[Obs, A, R, M])] =
    policy.choose(state).flatMap { a =>
      state
        .act(a)
        .map { case (r, s2) => (s2, (state, a, r)) }
    }

  /**
    Takes a static policy and a starting state and returns an M containing the
    final state and the trajectory that got us there.
    */
  def playEpisode[Obs, A, R, M[_]: Monad, T](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M],
      tracker: Tracker[Obs, A, R, T, M]
  ): M[(state.This, Trajectory[Obs, A, R, M])] =
    Util.iterateUntilM(state, tracker)(
      play(policy, _)
    )(_.isTerminal)

  /**
    Specialized version that keeps track of frequencies too.
    */
  def firstVisit[Obs, A, R, M[_]: Monad](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M]
  ): M[(state.This, Trajectory[Obs, A, R, M])] =
    playEpisode[Obs, A, R, M, FrequencyTracker[Snap[Obs, A, R, M], Obs]](
      policy,
      state,
      Tracker.firstVisit
    )

  /**
    We almost have all of the pieces. Now, to do it right, we need to zip back
    and forth along trajectories, updating the value function each time... maybe
    not updating the policy though.
    */
  def processTrajectory[Obs, A, R, G, M[_]](
      trajectory: Trajectory[Obs, A, R, M],
      valueFn: ActionValueFn[Obs, A, G],
      agg: MonoidAggregator[R, G, G]
  ): ActionValueFn[Obs, A, G] =
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    trajectory
      .foldLeft((valueFn, agg.monoid.zero)) {
        case ((vf, g), ((s, a, r), shouldUpdate)) =>
          val g2 = agg.append(g, r)
          if (shouldUpdate.get) {
            (vf.learn(s.observation, a, agg.present(g2)), g2)
          } else (vf, g2)
      }
      ._1

  /**
    So if you have G, your return... okay, this is a version that tracks the
    weights, but doesn't give you a nice way to push the weights back. What if
    we make the weight part of G? Try that in the next fn.
    */
  def processTrajectoryWeighted[Obs, A, R, G, M[_]](
      trajectory: Trajectory[Obs, A, R, M],
      valueFn: ActionValueFn[Obs, A, G],
      agg: MonoidAggregator[Snap[Obs, A, R, M], G, Option[G]]
  ): ActionValueFn[Obs, A, G] = {

    @tailrec
    def loop(
        t: Trajectory[Obs, A, R, M],
        vfn: ActionValueFn[Obs, A, G],
        g: G
    ): ActionValueFn[Obs, A, G] =
      if (t.isEmpty) vfn
      else {
        val (triple, shouldUpdate) = t.next
        agg.present(agg.append(g, triple)) match {
          case None => vfn
          case Some(g2) =>
            val newFn = if (shouldUpdate.get) {
              val (s, a, r) = triple
              vfn.learn(s.observation, a, g2)
            } else vfn
            loop(t, newFn, g2)
        }
      }
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    loop(trajectory, valueFn, agg.monoid.zero)
  }

  case class Weight(w: Double) extends AnyVal {
    def *(r: Weight): Weight = Weight(w + r.w)
  }

  object Weight {
    val one: Weight = Weight(1.0)
    val zero: Weight = Weight(0.0)
    implicit val monoid: Monoid[Weight] = Monoid.from(one)(_ * _)
  }

  // generates a monoid aggregator that can handle weights! We'll need to pair
  // this with a value function that knows how to handle weights on the way in,
  // by keeping a count for each state, and handling the weight multiplication,
  // that sort of thing.
  def weighted[Obs, A, R, G, M[_]](
      agg: MonoidAggregator[R, G, G],
      fn: (State[Obs, A, R, M], A, R) => Weight
  ): MonoidAggregator[Snap[Obs, A, R, M], (G, Weight), Option[(G, Weight)]] = {
    implicit val m: Monoid[G] = agg.monoid
    Aggregator
      .appendMonoid[Snap[Obs, A, R, M], (G, Weight)] {
        case ((g, w), (s, a, r)) =>
          (agg.append(g, r), w * fn(s, a, r))
      }
      .andThenPresent {
        case (g, Weight(0.0)) => None
        case pair             => Some(pair)

      }
  }

  // generates a function that uses two policies to assign a weight. This is
  // input into the stuff above.
  def byPolicy[Obs, A, R, M[_]](
      basePolicy: Policy[Obs, A, R, Cat, M],
      targetPolicy: Policy[Obs, A, R, Cat, M]
  ): (State[Obs, A, R, M], A, R) => Weight = {
    case (s, a, _) =>
      val num = targetPolicy.choose(s).pmf(a)
      val denom = basePolicy.choose(s).pmf(a)
      Weight(num / denom)
  }

  // function that always returns a weight of 1.a
  def constant[Obs, A, R, M[_]]: (State[Obs, A, R, M], A, R) => Weight = { case (s, a, r) => Weight.one }
}
