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
import com.stripe.rainier.core.{Categorical, Generator}

import scala.language.higherKinds

/**
  * Trait for things that can choose some Monadic result.
  */
trait Decider[A, -Obs, -R, M[+ _]] { self =>
  def choose(state: BaseState[A, Obs, R, M]): M[A]
}

trait Learner[A, -Obs, -R, M[+ _], This <: Learner[A, Obs, R, M, This]] {

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
  def learn(state: BaseState[A, Obs, R, M], action: A, reward: R): This
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
trait BasePolicy[A, -Obs, -R, M[+ _], This <: BasePolicy[A, Obs, R, M, This]]
    extends Learner[A, Obs, R, M, This]
    with Decider[A, Obs, R, M]

/**
  * Policy based on a discrete number of actions. This is a policy
  * where you can DIRECTLY UNDERSTAND what it's trying to do!
  *
  * TODO - note that for a state, it'd be great if you could get a
  * categorical distribution... but if you have a stochastic state at
  * least you can sample.
  */
trait CategoricalPolicy[A, -Obs, -R, This <: CategoricalPolicy[A, Obs, R, This]]
    extends BasePolicy[A, Obs, R, Generator, This] {
  def categories(state: BaseState[A, Obs, R, Generator]): Categorical[A]

  def choose(state: BaseState[A, Obs, R, Generator]): Generator[A] =
    categories(state).generator
}

object Policy {

  /**
    * Plays a single turn and returns a generator that returns the
    * reward and the next state. If the chosen state's not allowed,
    * returns the supplied penalty and sends the agent back to the
    * initial state.
    */
  def play[A, Obs, R, P <: Policy[A, Obs, R, P]](
      policy: P,
      state: State[A, Obs, R],
      penalty: R
  ): Generator[(P, R, State[A, Obs, R])] =
    for {
      a <- policy.choose(state)
      rs <- state.act(a).getOrElse(Generator.constant((penalty, state)))
    } yield (policy.learn(state, a, rs._1), rs._1, rs._2)

  /**
    * Returns the final policy, a sequence of the rewards received and
    * the final state.
    */
  def playN[A, Obs, R, P <: Policy[A, Obs, R, P]](
      policy: P,
      state: State[A, Obs, R],
      penalty: R,
      nTimes: Int
  ): Generator[(P, Seq[R], State[A, Obs, R])] =
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
  def playMany[A, Obs, R, P <: Policy[A, Obs, R, P]](
      pairs: List[(P, State[A, Obs, R])],
      penalty: R
  )(rewardSum: List[R] => R): Generator[(List[(P, State[A, Obs, R])], R)] =
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
  def playManyN[A, Obs, R, P <: Policy[A, Obs, R, P]](
      pairs: List[(P, State[A, Obs, R])],
      penalty: R,
      nTimes: Int
  )(rewardSum: List[R] => R): Generator[(List[(P, State[A, Obs, R])], List[R])] =
    Util.iterateM(nTimes)((pairs, List.empty[R])) {
      case (ps, rs) =>
        playMany(ps, penalty)(rewardSum).map {
          case (newPS, r) =>
            (newPS, rs :+ r)
        }
    }
}
