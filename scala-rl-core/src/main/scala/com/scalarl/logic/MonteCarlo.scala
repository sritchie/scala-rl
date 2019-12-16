/**
  Logic for playing episodic games.
  */
package com.scalarl
package logic

import cats.Monad
import cats.implicits._
import com.twitter.algebird.{Aggregator, Monoid, MonoidAggregator}
import com.scalarl.algebra.Weight
import com.scalarl.util.FrequencyTracker
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

  // TODO the next phase is to try and get n step SARSA working, maybe with an
  // expected bump at the end. And this has to work AS we're building the
  // trajectory.
  //
  // The trick here is that I need to freaking get access to the trajectory
  // itself and start to build up that business.
  //
  // Leave this for a while... but I think a key thing is going to be getting
  // access to the trajectory as I'm walking around this shit.
  def sarsa[Obs, A, R, M[_]: Monad, T](
      moment: Moment[Obs, A, R, M],
      tracker: MonteCarlo.Tracker[Obs, A, R, T, M]
  ): M[(Moment[Obs, A, R, M], MonteCarlo.Trajectory[Obs, A, R, M])] =
    Util.iterateUntilM(moment, tracker) {
      case m @ Moment(policy, state) =>
        m.play
    } { _.state.isTerminal }

  /**
    So if you have G, your return... okay, this is a version that tracks the
    weights, but doesn't give you a nice way to push the weights back. What if
    we make the weight part of G? Try that in the next fn.

    This is a full monte carlo trajectory tracker that's able to do off-policy
    control. The behavior policy does NOT change at all, but that's okay, I
    guess. We're going to have to solve that now. Presumably if you're updating
    a value function at any point you could get a new agent.
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
              vfn.update(s.observation, a, g2)
            } else vfn
            loop(t, newFn, g2)
        }
      }
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    loop(trajectory, valueFn, agg.monoid.zero)
  }

  /**
    This is a simpler version that doesn't do any weighting. This should be
    equivalent to the more difficult one above, with a constant weight of 1 for
    everything.
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
            (vf.update(s.observation, a, agg.present(g2)), g2)
          } else (vf, g2)
      }
      ._1

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
        case (g, Weight.Zero) => None
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
  def constant[Obs, A, R, M[_]]: SARS[Obs, A, R, M] => Weight = _ => Weight.One
}
