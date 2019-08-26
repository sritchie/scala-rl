package io.samritchie.rl

import com.stripe.rainier.core.{Categorical, Generator}
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import com.twitter.algebird.Monoid

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

  /**
    * Returns a policy that does NOT learn.
    */
  def uniformRand[A <: Action, R]: Policy[A, R] = RandomPolicy[A, R]
  def epsilonGreedy[A <: Action, R: Monoid: Ordering](epsilon: Double): Policy[A, R] =
    EpsilonGreedy[A, R](epsilon, Map.empty)
}

/**
  * Totally bullshit random policy.
  */
case class RandomPolicy[A <: Action, R]() extends Policy[A, R] {
  override def choose(state: State[A, R]): Generator[A] =
    Categorical.list(state.actions.toList).generator
}

/**
  * This is a version that accumulates the reward...
  *
  * @epsilon number between 0 and 1.
  */
case class EpsilonGreedy[A <: Action, R: Monoid: Ordering](epsilon: Double, rewards: Map[A, R])
    extends Policy[A, R] {

  /**
    * This doesn't necessarily break ties consistently. Check, and
    * note that we might want to break them randomly.
    */
  private def greedyAction: A = rewards.maxBy(_._2)._1

  override def choose(state: State[A, R]): Generator[A] =
    Util.epsilonGreedy(epsilon, greedyAction, state.actions).generator

  override def learn(state: State[A, R], action: A, reward: R): Policy[A, R] =
    copy(rewards = Monoid.plus(rewards, Map(action -> reward)))
}
