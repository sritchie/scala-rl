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
  import ConstantStep.zero
  import ConstantStepLaws.fill

  val EPS: Double = 1e-10
  val alpha: Double = 0.1

  implicit val stepMonoid: ConstantStepMonoid =
    new ConstantStepMonoid(alpha, EPS)

  implicit val equiv: Equiv[ConstantStep] =
    Equiv.fromFunction { (l, r) =>
      ((l.value == 0L) && (l.value == 0L)) || {
        approxEq(EPS)(l.value, r.value) && (l.time == r.time)
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
      approxEq(EPS)(simpleAcc, csAccumulator.decayTo(t, alpha, EPS).value)
    }
  })

  property("Adding two instances together acts like a single instance with double rewards")(check {
    forAll { (rewards: List[Float]) =>
      val (acc, ts1) = fill(stepMonoid, zero, rewards)
      val (doubleAcc, ts2) = fill(stepMonoid, zero, rewards.map(_.toDouble * 2))

      approxEq(EPS)(stepMonoid.plus(acc, acc).value, doubleAcc.value)
    }
  })

  property("With an alpha of one, all weight's placed on the latest reward.")(check {
    val oneMonoid = new ConstantStepMonoid(1.0, EPS)

    forAll { (rewards: List[Int]) =>
      val instances = rewards.scanLeft((zero, zero.time)) {
        case ((acc, ts), r) =>
          (oneMonoid.reward(acc, r, ts), ts + 1)
      }

      instances.tail.zip(rewards).forall {
        case ((acc, _), reward) =>
          approxEq(EPS)(acc.value, reward)
      }
    }
  })

  property("adding a reward works the same as adding an instance one tick later.")(check {
    forAll { (cs: ConstantStep, reward: Float) =>
      approxEq(EPS)(
        stepMonoid.reward(cs, reward, cs.time).value,
        stepMonoid.plus(cs, ConstantStep.build(reward, cs.time + 1)).value
      )
    }
  })
}

object ConstantStepLaws {
  def fill[T: Numeric](
      monoid: ConstantStepMonoid,
      init: ConstantStep,
      rewards: List[T]
  ): (ConstantStep, Long) =
    rewards
      .foldLeft((init, init.time)) {
        case ((acc, ts), r) =>
          (
            monoid
              .reward(acc, implicitly[Numeric[T]].toDouble(r), ts)
              .decayTo(ts + 1, monoid.alpha, monoid.eps),
            ts + 1
          )
      }
}

class ConstantStepTest extends org.scalatest.FunSuite {
  import BaseProperties.approxEq
  import ConstantStep.zero

  val EPS: Double = 1e-10
  val alpha: Double = 0.1
  val stepMonoid: ConstantStepMonoid = new ConstantStepMonoid(alpha, EPS)

  test("Two steps of the normal increment works as expected") {
    val r1 = 10
    val r2 = 12

    val stepOne = stepMonoid.reward(zero, r1, zero.time)
    val stepTwo = stepMonoid.reward(stepOne, r2, stepOne.time)

    assert(approxEq(EPS)(stepOne.value, alpha * r1))
    assert(approxEq(EPS)(stepTwo.value, (alpha * r1) + alpha * (r2 - (alpha * r1))))
  }
}

/**
  * Generators and Arbitrary instances live below.
  */
trait ConstantStepGen {
  def genStep: Gen[ConstantStep] =
    for {
      value <- Gen.choose(-1e100, 1e100)
      time <- Gen.choose(Int.MinValue.toLong, Int.MaxValue.toLong)
    } yield ConstantStep(value, time)
}

object ConstantStepGenerators extends ConstantStepGen

trait ConstantStepArb {
  import ConstantStepGenerators._

  implicit val arbStep: Arbitrary[ConstantStep] = Arbitrary(genStep)
}
