/**
  * Try using this to get rid of the bullshit f-bounded polymorphism:
  * https://tpolecat.github.io/2015/04/29/f-bounds.html
  *
  * And odersky's response for an even simpler way: https://gist.github.com/odersky/56323c309a186cffe9af
  */package io.samritchie.rl

import com.stripe.rainier.core.{Categorical, Generator}
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import com.twitter.algebird.Monoid

/**
  * This is how agents actually choose what comes next.
  */
trait Policy[A <: Action, R, P <: Policy[A, R, P]] {

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
  def learn(state: State[A, R], action: A, reward: R): P
}

object Policy {

  /**
    * Returns a policy that does NOT learn.
    */
  def uniformRand[A <: Action, R]: RandomPolicy[A, R] = RandomPolicy[A, R]

  def epsilonGreedy[A <: Action, R: Monoid: Ordering](epsilon: Double): EpsilonGreedy[A, R] =
    EpsilonGreedy[A, R](epsilon, Map.empty)

  /**
    * Same as the other arity, but allowed for
    */
  def epsilonGreedy[A <: Action, R: Monoid: Ordering](epsilon: Double, initial: R): EpsilonGreedy[A, R] =
    EpsilonGreedy[A, R](epsilon, Map.empty.withDefault(_ => initial))

}

/**
  * Totally bullshit random policy.
  */
case class RandomPolicy[A <: Action, R]() extends Policy[A, R, RandomPolicy[A, R]] {
  override def choose(state: State[A, R]): Generator[A] =
    Categorical.list(state.actions.toList).generator

  override def learn(state: State[A, R], action: A, reward: R): RandomPolicy[A, R] = this
}

/**
  * This is a version that accumulates the reward using a monoid.
  *
  * @epsilon number between 0 and 1.
  */
case class EpsilonGreedy[A <: Action, R: Monoid: Ordering](epsilon: Double, rewards: Map[A, R])
    extends Policy[A, R, EpsilonGreedy[A, R]] {

  /**
    * This doesn't necessarily break ties consistently. Check, and
    * note that we might want to break them randomly.
    */
  private def greedyAction: A = rewards.maxBy(_._2)._1

  override def choose(state: State[A, R]): Generator[A] =
    Util.epsilonGreedy(epsilon, greedyAction, state.actions).generator

  override def learn(state: State[A, R], action: A, reward: R): EpsilonGreedy[A, R] = {
    val oldV = rewards.getOrElse(action, Monoid.zero)
    copy(rewards = rewards + (action -> Monoid.plus(oldV, reward)))
  }
}

case class InstrumentedPolicy[A <: Action, R: Monoid: Ordering, P <: Policy[A, R, P]](policy: P, f: P => Map[A, R], acc: Map[A, List[R]])
    extends Policy[A, R, InstrumentedPolicy[A, R, P]] {
  override def choose(state: State[A, R]): Generator[A] = policy.choose(state)
  override def learn(state: State[A, R], action: A, reward: R): InstrumentedPolicy[A, R, P] = {
    val newPolicy: P = policy.learn(state, action, reward)
    val newR = f(newPolicy).getOrElse(action, Monoid.zero)
    val newV = acc.getOrElse(action, List.empty)
    InstrumentedPolicy(
      newPolicy,
      f,
      acc.updated(action, newV :+ newR)
    )
  }
}
