/**
  * Try using this to get rid of the bullshit f-bounded polymorphism:
  * https://tpolecat.github.io/2015/04/29/f-bounds.html
  *
  * And odersky's response for an even simpler way:
  * https://gist.github.com/odersky/56323c309a186cffe9af
  */
package io.samritchie.rl

import cats.implicits._
import com.stripe.rainier.cats._
import com.stripe.rainier.core.Generator

import scala.language.higherKinds

/**
  * Trait for things that can choose some Monadic result.
  */
trait Decider[A, R, M[_]] {
  def choose(state: State[A, R]): M[A]
}

trait StochasticDecider[A, R] extends Decider[A, R, Generator]

trait Learner[A, R, This <: Learner[A, R, This]] {

  /**
    * OR does this information go later? A particular policy should
    * get to witness the results of a decision... but instead of a
    * reward it might be a particular long term return.
    *
    * And each of the policies needs to have some array or something
    * that it is using to track all of these state values.
    *
    * SO THIS might not be great. But at this state, if you take this
    * action, you get this reward. That's the note.
    *
    * This of course might need to be a monadic response.
    */
  def learn(state: State[A, R], action: A, reward: R): This
}

/**
  * This is how agents actually choose what comes next. This is a
  * stochastic policy. We have to to be able to match this up with a
  * state that has the same monadic return type, but for now it's
  * hardcoded.
  *
  * A - Action
  * R - reward
  * This - policy
  */
trait Policy[A, R, This <: Policy[A, R, This]] extends Learner[A, R, This] with Decider[A, R, Generator]

object Policy {

  /**
    * Plays a single turn and returns a generator that returns the
    * reward and the next state. If the chosen state's not allowed,
    * returns the supplied penalty and sends the agent back to the
    * initial state.
    */
  def play[A, R, P <: Policy[A, R, P]](
      policy: P,
      state: State[A, R],
      penalty: R
  ): Generator[(P, R, State[A, R])] =
    for {
      a <- policy.choose(state)
      rs <- state.act(a).getOrElse(Generator.constant((penalty, state)))
    } yield (policy.learn(state, a, rs._1), rs._1, rs._2)

  /**
    * Returns the final policy, a sequence of the rewards received and
    * the final state.
    */
  def playN[A, R, P <: Policy[A, R, P]](
      policy: P,
      state: State[A, R],
      penalty: R,
      nTimes: Int
  ): Generator[(P, Seq[R], State[A, R])] =
    Util.iterateM(nTimes)((policy, Seq.empty[R], state)) {
      case (p, rs, s) =>
        play(p, s, penalty).map {
          case (newP, r, newS) =>
            (newP, rs :+ r, newS)
        }
    }

  /**
    * Takes an initial set of policies and a state...
    */
  def playMany[A, R, P <: Policy[A, R, P]](
      pairs: List[(P, State[A, R])],
      penalty: R
  )(rewardSum: List[R] => R): Generator[(List[(P, State[A, R])], R)] =
    pairs.toList
      .traverse {
        case (p, s) => play(p, s, penalty)
      }
      .map { results =>
        (results.map { case (a, b, c) => (a, c) }, rewardSum(results.map(_._2)))
      }

  /**
    * Takes an initial set of policies and a state...
    */
  def playManyN[A, R, P <: Policy[A, R, P]](
      pairs: List[(P, State[A, R])],
      penalty: R,
      nTimes: Int
  )(rewardSum: List[R] => R): Generator[(List[(P, State[A, R])], List[R])] =
    Util.iterateM(nTimes)((pairs, List.empty[R])) {
      case (ps, rs) =>
        playMany(ps, penalty)(rewardSum).map {
          case (newPS, r) =>
            (newPS, rs :+ r)
        }
    }
}
