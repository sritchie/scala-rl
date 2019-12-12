/**
  Monte Carlo methods. This will be tough... a second layer of aggregation as we
  move through episodes and aggregate across game.
  */
package io.samritchie.rl
package book

import cats.{Id, Monad}
import cats.implicits._
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.core.Generator
import com.stripe.rainier.sampler.RNG
import com.twitter.algebird.{Aggregator, AveragedValue, MonoidAggregator}
import com.twitter.util.Stopwatch
import io.samritchie.rl.logic.MonteCarlo
import io.samritchie.rl.policy.{Greedy, Random}
import io.samritchie.rl.util.CardDeck
import io.samritchie.rl.world.Blackjack

object Chapter5 {
  import Blackjack.{Action, AgentView, Result}

  implicit val rng: RNG = RNG.default
  implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)

  def stickHigh[S[_]](hitBelow: Int): Policy[AgentView, Action, Double, Id, S] =
    new Policy[AgentView, Action, Double, Id, S] {
      override def choose(state: State[AgentView, Action, Double, S]): Action = {
        val score = state.observation.playerSum
        if (score < 20) Action.Hit else Action.Stay
      }
    }

  // I need it in this form for off-policy sampling, etc... but to do the
  // stochastic thing I need to be able to get a generator out of it. Or some
  // monadic thing that does not let me get expected value.
  def stickHighCat[S[_]](hitBelow: Int): Policy[AgentView, Action, Double, Cat, S] =
    stickHigh(hitBelow).mapK(Util.idToMonad[Cat])

  def random[M[_]]: Policy[AgentView, Action, Double, Cat, M] = Random()

  /**
    Is this appreciably slower? This is going to be useful, in any case, when I'm working with the tests.
    */
  def limitedM[M[_]: Monad](state: M[Blackjack[M]]): M[State[AgentView, Action, Double, M]] =
    state.map(_.mapObservation(_.agentView).mapReward {
      case Result.Draw | Result.Pending => 0
      case Result.Win                   => 1
      case Result.Lose                  => -1
    })

  val starter: Generator[Blackjack[Generator]] =
    Blackjack.Config[Generator](CardDeck.basic).stateM

  // this obviously needs a better name BUT this is the generate that pops out
  // uniform distributions over
  val uniformFigureThree: Generator[Blackjack[Generator]] =
    Blackjack.Config[Generator](CardDeck.basic).stateM

  val limited: Generator[State[AgentView, Action, Double, Generator]] =
    limitedM(starter)

  type Loop[M[_], T] = T => M[T]

  // This returns a function that will take a value function and return a new M
  // of a value function that's calculated by playing a game and then pushing
  // back state through the policy.
  //
  // Inside, the policy's getting generated from the NEW value function. There's
  // probably some clearer way to write this thing.
  def updateFn[Obs, A, R, T, M[_]: Monad](
      g: M[State[Obs, A, R, M]],
      agg: MonoidAggregator[R, T, T],
      policyFn: ActionValueFn[Obs, A, T] => Policy[
        Obs,
        A,
        R,
        M,
        M
      ]
  ): Loop[M, ActionValueFn[Obs, A, T]] = {

    // This will start with the limited world and get a single state... then run
    // the first visit tracker. This means that it will play an entire episode,
    // tracking frequencies.

    // This is only important because it keeps track of the frequencies that you
    // saw each state; it needs this when you go walk the trajectory later, so
    // you can only apply the goods on a first visit.
    def playGen(policy: Policy[Obs, A, R, M, M]) =
      g.flatMap { state =>
        MonteCarlo
          .firstVisit[Obs, A, R, M](
            policy,
            state
          )
      }

    def loop(vfn: ActionValueFn[Obs, A, T]) =
      playGen(policyFn(vfn)).map {
        case (_, trajectory) =>
          // Here we process the trajectory backwards and get ourselves a new action
          // value function. And that's it! The action value function work that I was
          // doing with the bandits was really preparation for the next chapter, and
          // shouldn't have taken up so much time here.
          //
          // I think from here... I should make sure that this plays correctly, then
          // not worry too much about the actual charts. Once I get the abstraction
          // fully locked down I can go build the charts in Python.
          MonteCarlo.processTrajectory[Obs, A, R, T](
            trajectory,
            vfn,
            agg
          )
      }

    loop
  }

  /**
    This is the figure that explores the stickHigh strategy over a bunch of
    states, tracking what happens with a usable ace and with no usable ace.
    */
  def figureFiveOne(): Unit = {
    println("Hello, chapter 5!")
    println("Let's play blackjack!")

    val policy = stickHigh[Generator](hitBelow = 20).mapK(Util.idToMonad[Generator])
    val agg = Aggregator.fromMonoid[Double]
    val fn = updateFn[AgentView, Action, Double, Double, Generator](limited, agg, _ => policy)
    val base: ActionValueFn[AgentView, Action, Double] = value.ActionValueMap(
      Map.empty,
      0.0
    )
    val elapsed = Stopwatch.start()
    Util.iterateM(10000)(base)(fn).get
    val t1 = elapsed()
    println(s"Time to play 10000 runs of blackjack: ${t1}")

    Util.iterateM(500000)(base)(fn).get
    val t2 = elapsed()
    println(s"Time to play 500000 runs of blackjack: ${t2}")
    ()
  }

  /**
    This uses exploring starts to capture the optimal policy.

    - go through a single round of the game, then
    - update the policy to use the new function.

    the policy gets updated on every play at the end of the trajectory walk;
    */
  def figureFiveTwo(): Unit = {
    import Util.Instances.averageValueOrd

    val base: ActionValueFn[AgentView, Action, AveragedValue] = value.ActionValueMap(
      Map.empty,
      AveragedValue(0.0)
    )
    val agg = Aggregator.prepareMonoid[Double, AveragedValue](AveragedValue(_))
    val fn = updateFn[AgentView, Action, Double, AveragedValue, Generator](
      limited,
      agg, { vfn =>
        val evaluator =
          Evaluator.ActionValue.fn[AgentView, Action, Double, AveragedValue, Generator](vfn)
        new Greedy(evaluator, 0.0).mapK(Cat.catToGenerator)
      }
    )

    val elapsed = Stopwatch.start()
    Util.iterateM(500000)(base)(fn).get
    val t1 = elapsed()
    println(s"Time to play 500000 runs of blackjack: ${t1}")
    ()
  }

  /**
    this checks using the random policy to check the stickHigh behavior policy,
    and compares ordinary and weighted off-policy sampling.
    */
  def figureFiveThree(): Unit = {
    val all: Vector[CardDeck.Card] = for {
      suit <- CardDeck.Suit.all
      rank <- CardDeck.Rank.all
    } yield CardDeck.Card(suit, rank)

    // first step is to get the custom card deck going.

  }

  def main(items: Array[String]): Unit = {
    println("Hello, chapter 5!")
    println("Let's play blackjack!")
    figureFiveTwo()
    ()
  }
}
