package io.samritchie.rl

import com.stripe.rainier.core._
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import scala.collection.SortedSet
import scala.util.Try

/**
  * Okay, let's see what we can do for this bandit thing. We want
  * something that can run as a bandit, and then a world to run it in.
  *
  *
  * FIRST STEP:
  *
  * - Recreate figure 2_2, playing the bandit for a while. see what happens.
  * - epsilon-greedy
  * - ucb
  * - gradient
  *
  * Maybe.... get it working, then convert it to Rainier?? That would
  * be wild. I'm going to give myself two weeks to get this code
  * written. A sprint. Not so bad.
  *
  * Replace the Rainier stuff with the probability
  * monad. https://github.com/jliszka/probability-monad/blob/9c988434c423e24461dc7543344461f900e36d46/src/main/scala/probability-monad/Distribution.scala
  *
  * Article about this shit: https://www.chrisstucchio.com/blog/2016/probability_the_monad.html
  *
  *
  * Notes:
  * - Functional graph: https://manuel.kiessling.net/2016/02/15/scala-traversing-a-graph-in-a-functional-way/
  * - scala-graph
  * verizon's quiver library
  */

trait Action

/**
  * A world should probably have a generator of states and
  * actions... and then you can use that to get to the next bullshit.
  */
trait State[A <: Action, Reward] {
  /**
    * For every action you could take, returns a generator of the next set of rewards
    */
  def dynamics: Map[A, Generator[(Reward, State[A, Reward])]]

  /**
    * Return None if it's an invalid action. We need a better way to
    * deal with this.
    */
  def act(action: A): Option[Generator[(Reward, State[A, Reward])]] = dynamics.get(action)
  def actions: Set[A] = dynamics.keySet
}

/**
  * Then we have a bandit... a single state thing.
  */
object State {
  /**
    * Nice easy way to create the goodies.
    */
  def fromMap[A <: Action: Ordering, R](dynamics: Map[A, Generator[(R, State[A, R])]]): State[A, R] =
    MapState[A, R](dynamics)

  /**
    * A bandit is just a single state.
    */
  def bandit[A <: Action: Ordering, R](rewards: Map[A, Generator[R]]): State[A, R] =
    Bandit[A, R](rewards)
}

/**
  * MDP with state derived from a map.
  *
  * This is not a great interface, since we end up going BACK
  */
case class MapState[A <: Action, R](dynamics: Map[A, Generator[(R, State[A, R])]]) extends State[A, R]

/**
  * MDP with a single state.
  */
case class Bandit[A <: Action, R](rewards: Map[A, Generator[R]]) extends State[A, R] {
  override def dynamics = rewards.mapValues(_.map(r => (r, this)))
}

/**
  * This is how agents actually choose what comes next.
  */
trait Policy[A <: Action, R] {
  /**
    * This gets me my action.
    */
  def choose(state: State[A, R]): Generator[A]

  /**
    * TODO I think this is what we need. A functional way to absorb
    * new information.
    *
    * OR does this information go later? A particular policy should
    * get to witness the results of a decision... but instead of a
    * reward it might be a particular long term return.
    *
    * And each of the policies needs to have some array or something
    * that it is using to track all of these state values.
    *
    * SO THIS might not be great.
    */
  def learn(state: State[A, R], action: A, reward: R): Policy[A, R] = this
}

object Policy {
  def uniformRand[A <: Action, R]: Policy[A, R] = RandomPolicy[A, R]()
}

case class RandomPolicy[A <: Action, R]() extends Policy[A, R] {
  override def choose(state: State[A, R]): Generator[A] =
    Categorical.list(state.actions.toList).generator
}

object ValueFunction {

}

/**
  * TODO Figure out how to make something that tracks action and state
  * values here.
  */
trait ValueFunction {
}

object Game {
  /**
    * These are needed to actually call get on anything.
    */
  implicit val rng: RNG = RNG.default
  implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)

  /**
    * Plays a single turn and returns a gene
    */
  def play[A <: Action](state: State[A, Float], policy: Policy[A, Float]): Generator[(Float, State[A, Float])] =
    policy.choose(state).flatMap { a =>
      state.act(a).getOrElse(
        Generator.constant((0f, state))
      )
    }
}

object FirstExample {
  /**
    * I think I'm in a place where I can actually play that first game!
    */
}
