package io.samritchie.rl
package util

package io.samritchie.connectfour

import com.twitter.algebird._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.forAll

class ConstantStepLaws extends PropSpec with Checkers with ConstantStepArb {
  import BaseProperties._
  import ConstantStep.{zero, Alpha}
  import ConstantStepLaws.{alpha, fill, EPS}

  implicit val stepMonoid: ConstantStepMonoid =
    new ConstantStepMonoid(alpha, EPS)

  implicit val equiv: Equiv[ConstantStep] =
    Equiv.fromFunction { (l, r) =>
      ((l.value == 0L) && (l.value == 0L)) || {
        approxEq(EPS.toDouble)(l.value, r.value) && (l.time == r.time)
      }
    }

  property("ConstantStep forms a commutative monoid")(check {
    monoidLaws[ConstantStep] && isCommutative[ConstantStep]
  })

  property("ConstantStep's monoid works like the single-step version")(check {
    forAll { (rewards: List[Float]) =>
      val (csAccumulator, t) = fill(stepMonoid, zero, rewards)
      val simpleAcc = rewards.foldLeft(0.0) {
        case (acc, reward) =>
          acc + alpha * (reward - acc)
      }
      approxEq(EPS.toDouble)(simpleAcc, csAccumulator.decayTo(t, alpha, EPS).value)
    }
  })

  property("Adding two instances together acts like a single instance with double rewards")(check {
    forAll { (rewards: List[Float]) =>
      val (acc, ts1) = fill(stepMonoid, zero, rewards)
      val (doubleAcc, ts2) = fill(stepMonoid, zero, rewards.map(_.toDouble * 2))

      approxEq(EPS.toDouble)(stepMonoid.plus(acc, acc).value, doubleAcc.value)
    }
  })

  property("With an alpha of one, all weight's placed on the latest reward.")(check {
    val oneMonoid = new ConstantStepMonoid(Alpha(1.0), EPS)

    forAll { (rewards: List[Int]) =>
      val instances = rewards.scanLeft((zero, zero.time)) {
        case ((acc, ts), r) =>
          (oneMonoid.reward(acc, r, ts), ts.tick)
      }

      instances.tail.zip(rewards).forall {
        case ((acc, _), reward) =>
          approxEq(EPS.toDouble)(acc.value, reward)
      }
    }
  })

  property("adding a reward works the same as adding an instance one tick later.")(check {
    forAll { (cs: ConstantStep, reward: Double) =>
      approxEq(EPS.toDouble)(
        stepMonoid.reward(cs, reward, cs.time).value,
        stepMonoid.plus(cs, ConstantStep.buildAggregate(alpha * reward, cs.time.tick)).value
      )
    }
  })

  property("A reward is an aggregate * alpha, one step in the future.")(check {
    forAll { (reward: Double, time: Time) =>
      approxEq(EPS.toDouble)(
        ConstantStep.buildReward(reward, alpha, time).value,
        ConstantStep.buildAggregate(alpha * reward, time.tick).value
      )
    }
  })
}

object ConstantStepLaws {
  import ConstantStep.{Alpha, Epsilon}

  val EPS: Epsilon = Epsilon(1e-10)
  val alpha: Alpha = Alpha(0.1)
  val stepMonoid: ConstantStepMonoid =
    new ConstantStepMonoid(alpha, EPS)

  def fill[T: Numeric](
      monoid: ConstantStepMonoid,
      init: ConstantStep,
      rewards: List[T]
  ): (ConstantStep, Time) =
    rewards
      .foldLeft((init, init.time)) {
        case ((acc, ts), r) =>
          (
            monoid
              .reward(acc, implicitly[Numeric[T]].toDouble(r), ts)
              .decayTo(ts.tick, monoid.alpha, monoid.eps),
            ts.tick
          )
      }
}

class ConstantStepTest extends org.scalatest.FunSuite {
  import BaseProperties.approxEq
  import ConstantStep.zero
  import ConstantStepLaws.{alpha, stepMonoid, EPS}

  test("Two steps of the normal increment works as expected") {
    val r1 = 10
    val r2 = 12

    val stepOne = stepMonoid.reward(zero, r1, zero.time)
    val stepTwo = stepMonoid.reward(stepOne, r2, stepOne.time)

    assert(approxEq(EPS.toDouble)(stepOne.value, alpha * r1))
    assert(approxEq(EPS.toDouble)(stepTwo.value, (alpha * r1) + alpha * (r2 - (alpha * r1))))
  }

  test("monoid works like the single-step version") {
    val rewards = List(5.1671178e-20, -1.671406e-38)
    val (csAccumulator, t) = ConstantStepLaws.fill(stepMonoid, zero, rewards)
    val simpleAcc = rewards.foldLeft(0.0) {
      case (acc, reward) =>
        acc + alpha * (reward - acc)
    }

    // TODO check here for some overflow bullshit and fix up the tests
    // to not do that.
    println(simpleAcc)
    println(csAccumulator.decayTo(t, alpha, EPS))
    assert(approxEq(EPS.toDouble)(simpleAcc, csAccumulator.decayTo(t, alpha, EPS).value))
  }

  test("Adding an instance with (alpha * reward) one tick in the future equals a reward now.") {
    val r: Double = 10.0

    val rewarded = stepMonoid.reward(zero, r, zero.time)
    val stepped = stepMonoid.plus(zero, ConstantStep(alpha * r, zero.time.tick))

    assert(approxEq(EPS.toDouble)(rewarded.value, stepped.value))
  }
}

/**
  * Generators and Arbitrary instances live below.
  */
trait ConstantStepGen {
  def genTime: Gen[Time] =
    Gen
      .choose(Int.MinValue.toLong, Int.MaxValue.toLong)
      .map(Time(_))

  def genStep: Gen[ConstantStep] =
    for {
      value <- Gen.choose(-1e50, 1e50)
      time <- genTime
    } yield ConstantStep(value, time)
}

object ConstantStepGenerators extends ConstantStepGen

trait ConstantStepArb {
  import ConstantStepGenerators._

  implicit val arbStep: Arbitrary[ConstantStep] = Arbitrary(genStep)
  implicit val arbTime: Arbitrary[Time] = Arbitrary(genTime)
}
