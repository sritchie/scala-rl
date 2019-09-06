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

  def fill(monoid: ConstantStepMonoid, init: ConstantStep, rewards: List[Double]): ConstantStep =
    (rewards
      .foldLeft((init, init.time)) {
        case ((acc, ts), r) =>
          (stepMonoid.reward(acc, r, ts), ts + 1)
      })
      ._1

  property("ConstantStep forms a commutative monoid")(check {
    monoidLaws[ConstantStep] && isCommutative[ConstantStep]
  })

  property("ConstantStep's monoid works like the single-step version")(check {
    forAll { (rewards: List[Double]) =>
      val simpleAcc = rewards.foldLeft(0.0) {
        case (acc, reward) =>
          acc + alpha * (reward - acc)
      }
      val csAcc = fill(stepMonoid, ConstantStep.zero, rewards)

      approxEq(EPS)(simpleAcc, csAcc.value)
    }
  })

  property("Adding two instances together acts like a single instance with double rewards")(check {
    forAll { (rewards: List[Double]) =>
      val zero = ConstantStep.zero
      val acc = fill(stepMonoid, zero, rewards)
      val doubleAcc = fill(stepMonoid, zero, rewards.map(_ * 2))

      approxEq(EPS)(stepMonoid.plus(acc, acc).value, doubleAcc.value)
    }
  })

  property("With an alpha of one, all weight's placed on the latest reward.")(check {
    val oneMonoid = new ConstantStepMonoid(1.0, EPS)
    val zero = ConstantStep.zero

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
}

class ConstantStepTest extends org.scalatest.FunSuite {
  import BaseProperties.approxEq

  val EPS: Double = 1e-10
  val alpha: Double = 1

  test("Two steps of the normal increment works as expected") {
    val stepMonoid: ConstantStepMonoid = new ConstantStepMonoid(alpha, EPS)
    val zero = ConstantStep.zero

    val r1 = 10
    val r2 = 12

    val stepOne = stepMonoid
      .reward(zero, r1, zero.time)
      .decayTo(zero.time + 1, alpha, EPS)

    val stepTwo = stepMonoid
      .reward(stepOne, r2, stepOne.time)
      .decayTo(stepOne.time + 1, alpha, EPS)

    approxEq(EPS)(stepOne.value, alpha * r1)
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
