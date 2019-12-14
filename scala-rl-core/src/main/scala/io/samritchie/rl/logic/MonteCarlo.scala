/**
  Logic for playing episodic games.
  */
package io.samritchie.rl
package logic

import cats.Monad
import cats.implicits._
import com.twitter.algebird.{Aggregator, Monoid, MonoidAggregator}
import io.samritchie.rl.util.{FrequencyTracker, Weight}
import scala.annotation.tailrec

object MonteCarlo {
  import Episode.Moment

  case class ShouldUpdateState(get: Boolean) extends AnyVal
  object ShouldUpdateState {
    val yes = ShouldUpdateState(true)
    val no = ShouldUpdateState(false)
  }

  // This is an iterator of SARS observations, starting at the very beginning,
  // paired with a note about whether or not that observation should trigger a
  // state update. This is primarily interesting for distinguishing between
  // first and every visit updates, or something in between.
  type Trajectory[Obs, A, R, M[_]] = Iterator[(SARS[Obs, A, R, M], ShouldUpdateState)]

  // The T type here is type that's used to aggregate the trajectory.
  type Tracker[Obs, A, R, T, M[_]] = MonoidAggregator[SARS[Obs, A, R, M], T, Trajectory[Obs, A, R, M]]

  object Tracker {
    // Use a frequency tracker to aggregate; this allows us to count backwards
    // from the end and figure out when the first time we saw a particular state
    // was.
    type FirstVisit[Obs, A, R, M[_]] = Tracker[Obs, A, R, FrequencyTracker[SARS[Obs, A, R, M], Obs], M]

    // If we don't care we can accumulate the trajectory the state using a vector.
    //
    // This should be equivalent to using a frequency tracker but ignoring
    // whether or not we've seen the state.
    type EveryVisit[Obs, A, R, M[_]] = Tracker[Obs, A, R, Vector[SARS[Obs, A, R, M]], M]

    /**
      Returns a Tracker instance that will generate a trajectory where
      ShouldUpdateState is only true the first time in the trajectory a state is
      encountered.
      */
    def firstVisit[Obs, A, R, M[_]]: FirstVisit[Obs, A, R, M] = {
      implicit val m = FrequencyTracker.monoid[SARS[Obs, A, R, M], Obs](_.state.observation)
      Aggregator.appendMonoid(
        appnd = _ :+ _,
        pres = _.reverseIterator.map { case (t, seen) => (t, ShouldUpdateState(seen == 0)) }
      )
    }

    /**
      Returns a Tracker instance where ShouldUpdateState signals YES for every
      single state.
      */
    def everyVisit[Obs, A, R, M[_]]: EveryVisit[Obs, A, R, M] =
      Aggregator.appendMonoid(
        appnd = _ :+ _,
        pres = _.reverseIterator.map((_, ShouldUpdateState.yes))
      )
  }

  /**
    We almost have all of the pieces. Now, to do it right, we need to zip back
    and forth along trajectories, updating the value function each time... maybe
    not updating the policy though.

    valueFn is tracking the type that we accumulate, the returns, as we
    accumulate them back along the trajectory. Internally it might do some other
    agg, of course.
    */
  def processTrajectorySimple[Obs, A, R, G, M[_]](
      trajectory: Trajectory[Obs, A, R, M],
      valueFn: ActionValueFn[Obs, A, G],
      agg: MonoidAggregator[R, G, G]
  ): ActionValueFn[Obs, A, G] =
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    trajectory
      .foldLeft((valueFn, agg.monoid.zero)) {
        case ((vf, g), (SARS(s, a, r, s2), shouldUpdate)) =>
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
  def processTrajectory[Obs, A, R, G, M[_]](
      trajectory: Trajectory[Obs, A, R, M],
      valueFn: ActionValueFn[Obs, A, G],
      agg: MonoidAggregator[SARS[Obs, A, R, M], G, Option[G]]
  ): ActionValueFn[Obs, A, G] = {

    @tailrec
    def loop(
        t: Trajectory[Obs, A, R, M],
        vfn: ActionValueFn[Obs, A, G],
        g: G
    ): ActionValueFn[Obs, A, G] =
      if (t.isEmpty) vfn
      else {
        val (sars, shouldUpdate) = t.next
        agg.present(agg.append(g, sars)) match {
          case None => vfn
          case Some(g2) =>
            val newFn = if (shouldUpdate.get) {
              val SARS(s, a, r, s2) = sars
              vfn.learn(s.observation, a, g2)
            } else vfn
            loop(t, newFn, g2)
        }
      }
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    loop(trajectory, valueFn, agg.monoid.zero)
  }

  // generates a monoid aggregator that can handle weights! We'll need to pair
  // this with a value function that knows how to handle weights on the way in,
  // by keeping a count for each state, and handling the weight multiplication,
  // that sort of thing.
  def weighted[Obs, A, R, G, M[_]](
      agg: MonoidAggregator[R, G, G],
      fn: SARS[Obs, A, R, M] => Weight
  ): MonoidAggregator[SARS[Obs, A, R, M], (G, Weight), Option[(G, Weight)]] = {
    implicit val m: Monoid[G] = agg.monoid
    Aggregator
      .appendMonoid[SARS[Obs, A, R, M], (G, Weight)] {
        case ((g, w), sars) =>
          (agg.append(g, sars.reward), w * fn(sars))
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

  // function that always returns a weight of 1.
  def constant[Obs, A, R, M[_]]: SARS[Obs, A, R, M] => Weight = _ => Weight.one
}
