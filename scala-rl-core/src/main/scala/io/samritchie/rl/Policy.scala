/**
  * Try using this to get rid of the bullshit f-bounded polymorphism:
  * https://tpolecat.github.io/2015/04/29/f-bounds.html
  *
  * And odersky's response for an even simpler way:
  * https://gist.github.com/odersky/56323c309a186cffe9af
  *
  */
package io.samritchie.rl

import cats.Eval
import cats.implicits._
import cats.arrow.FunctionK
import com.stripe.rainier.cats._
import com.stripe.rainier.core.{Categorical, Generator}

import scala.language.higherKinds

/**
  * This is how agents actually choose what comes next. This is a
  * stochastic policy. We have to to be able to match this up with a
  * state that has the same monadic return type, but for now it's
  * hardcoded.
  *
  * A - Action
  * Obs - the observation offered by this state.
  * R - reward
  * M - the monadic type offered by the policy.
  * S - the monad for the state.
  * This - policy
  */
trait BasePolicy[A, -Obs, -R, M[+ _], S[+ _], This <: BasePolicy[A, Obs, R, M, S, This]] { self =>
  def choose(state: BaseState[A, Obs, R, S]): M[A]

  /**
    TODO Note - I can imagine that we wouldn't want to present a reward,
    necessarily, but some aggregated thing.
    */
  def learn(state: BaseState[A, Obs, R, S], action: A, reward: R): This
}

/**
  * Policy based on a discrete number of actions. This is a policy
  * where you can DIRECTLY UNDERSTAND what it's trying to do!
  *
  * TODO - note that for a state, it'd be great if you could get a
  * categorical distribution... but if you have a stochastic state at
  * least you can sample.
  *
  * TODO - this really just needs to extend Decider... not Learner. If
  * it's not learning, you can just lock it in.
  */
trait BaseCategoricalPolicy[A, -Obs, -R, S[+ _], This <: BaseCategoricalPolicy[A, Obs, R, S, This]]
    extends BasePolicy[A, Obs, R, Generator, S, This] {
  def categories(state: BaseState[A, Obs, R, S]): Categorical[A]
  def choose(state: BaseState[A, Obs, R, S]): Generator[A] =
    categories(state).generator
}

trait CategoricalPolicy[A, -Obs, -R, This <: CategoricalPolicy[A, Obs, R, This]]
    extends BaseCategoricalPolicy[A, Obs, R, Generator, This] {
  val fk = FunctionK.id
}

trait CategoricalNowPolicy[A, -Obs, -R, This <: CategoricalNowPolicy[A, Obs, R, This]]
    extends BaseCategoricalPolicy[A, Obs, R, Eval, This] {
  val fk: FunctionK[Eval, Generator] = new FunctionK[Eval, Generator] {
    def apply[B](ev: Eval[B]): Generator[B] =
      Generator.constant(ev.value)
  }
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
