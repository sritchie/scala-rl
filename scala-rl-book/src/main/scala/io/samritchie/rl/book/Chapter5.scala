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

  val limited: Generator[State[AgentView, Action, Double, Generator]] =
    limitedM(starter)

  val uniformStarts: Generator[Blackjack[Generator]] = {
    val aceOfSpades = CardDeck.Card(CardDeck.Suit.Spades, CardDeck.Rank.Ace)
    val five = CardDeck.Card(CardDeck.Suit.Spades, CardDeck.Rank.Number(5))
    val six = CardDeck.Card(CardDeck.Suit.Spades, CardDeck.Rank.Number(6))

    // Generate a uniform hand for the player with a score from 11-21. The
    // actual cards don't really matter for this game.
    val uniformPlayerHand: Generator[Blackjack.Hand] = for {
      usableAce <- Generator.vector(Vector(true, false))
      total <- Generator.vector((11 to 21).toVector)
    } yield {
      val card = CardDeck.Card(CardDeck.Suit.Spades, CardDeck.Rank.Number(total - 11))
      val showing = if (usableAce) Seq(aceOfSpades, card) else Seq(five, six, card)
      Blackjack.Hand(
        showing,
        Seq.empty
      )
    }

    // dealer hand is the same as in any game.
    val dealerHand: Generator[Blackjack.Hand] =
      Blackjack.dealerHand(CardDeck.basic)

    val conf = Blackjack.Config[Generator](CardDeck.basic)

    val gameGenerator: Generator[Blackjack.Game] = for {
      dealer <- dealerHand
      player <- uniformPlayerHand
    } yield Blackjack.Game(player, dealer)

    gameGenerator.map(conf.build(_))
  }

  type Loop[M[_], T] = T => M[T]

  // This returns a function that will take a value function and return a new M
  // of a value function that's calculated by playing a game and then pushing
  // back state through the policy.
  //
  // Inside, the policy's getting generated from the NEW value function. There's
  // probably some clearer way to write this thing.
  //
  // OKAY, so how does this relate?
  //
  // If you have a constant policy that does NOT get updated, then you've solved
  // prediction. you're evaluating some policy.
  //
  // then we do exploring starts, if you toggle g to have it send out uniform
  // states, and have a policy function that updates. If you do exploring starts
  // you can use a fully greedy policy and count on the exploring starts to help
  // you explore the full space.
  //
  // How do you relax the exploring starts?
  //
  // One way is to use an epsilon soft policy...
  //
  // but then we can use a greedy policy and lock down the importance sampling
  // thing. This is where my derivation is going to get involved.
  //
  // The good stuff!
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
      limitedM(uniformStarts),
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

    // What's missing here is actually plotting the behavior of the optimal
    // policy... I'm getting convinced that plotting in Scala is a waste of
    // time. I'm going to wait.
    ()
  }

  /**
    this checks using the random policy to check the stickHigh behavior policy,
    and compares ordinary and weighted off-policy sampling.
    */
  def figureFiveThree(): Unit = {
    // start with a static hand, the same as they used to generate the graph.
    //
    // From the textbook: "The value of this state under the target policy is
    // approximately -0.27726 (this was determined by separately generating
    // one-hundred million episodes using the target policy and averaging their
    // returns)"
    val trueValue = -0.27726
    val playerHand = Blackjack.Hand(
      Seq(
        CardDeck.Card(CardDeck.Suit.Spades, CardDeck.Rank.Ace),
        CardDeck.Card(CardDeck.Suit.Spades, CardDeck.Rank.Number(2))
      ),
      Seq.empty
    )

    // dealer hand is the same as in any game.
    val dealerHand: Generator[Blackjack.Hand] =
      Blackjack.dealerHand(CardDeck.basic)

    // config deals cards like normal once the game starts.
    val conf = Blackjack.Config[Generator](CardDeck.basic)

    // generator that starts in the prescribed state, always.
    val blackjackGen: Generator[Blackjack[Generator]] =
      dealerHand
        .map(Blackjack.Game(playerHand, _))
        .map(conf.build(_))

    // the behavior policy is the random policy. We're going to randomly explore
    // and see what we get.
    val behavior = random[Generator].mapK(Cat.catToGenerator)

    // then we're going to apply those results to the target policy.
    val target = stickHigh[Generator](hitBelow = 20).mapK(Util.idToMonad[Generator])

    // I think the trick is that we need to have two CATEGORICAL policies that
    // we can use to interact with the generator world.
    ()
  }

  def main(items: Array[String]): Unit = {
    println("Hello, chapter 5!")
    println("Let's play blackjack!")
    figureFiveThree()

    // TODO off policy MC prediction...
    // TODO off policy MC control
    ()
  }
}
