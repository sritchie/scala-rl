package io.samritchie.rl
package util

import com.twitter.algebird.Group

/**
  * Exponential recency-weighted average. This is similar to a
  * weighted average, but instead of weighting by the count, it uses a
  * constant weighting factor.

  TODO consider changing Numeric to ToDouble?
  */
case class ConstantStep(value: Double, time: Time) extends Ordered[ConstantStep] {
  import ConstantStep.{Alpha, Epsilon}

  def compare(that: ConstantStep): Int =
    time.compareTo(that.time) match {
      case 0     => value.compare(that.value)
      case other => other
    }

  def decayTo(t2: Time, alpha: Alpha, eps: Epsilon): ConstantStep =
    if (t2 <= time)
      this
    else {
      val newV = value * math.pow(1 - alpha.toDouble, t2 - time)
      if (math.abs(newV) > eps.toDouble)
        ConstantStep(newV, t2)
      else
        ConstantStep.zero
    }
}

object ConstantStep {
  case class Alpha(toDouble: Double) extends AnyVal {
    def *(r: Double): Double = toDouble * r
  }
  case class Epsilon(toDouble: Double) extends AnyVal

  val zero: ConstantStep = ConstantStep(0.0, Time.Min)

  def buildAggregate[T](value: T)(implicit num: Numeric[T]): ConstantStep =
    buildAggregate(value, Time.Min)

  def buildAggregate[T](value: T, time: Time)(implicit num: Numeric[T]): ConstantStep =
    ConstantStep(num.toDouble(value), time)

  /**
    * Rewards can only be assigned to time one tick in the future.
    */
  def buildReward[T](reward: T, alpha: Alpha, time: Time)(implicit num: Numeric[T]): ConstantStep =
    ConstantStep(alpha * num.toDouble(reward), time.tick)

  def group(alpha: Alpha, eps: Epsilon): Group[ConstantStep] =
    new ConstantStepGroup(alpha, eps)

  implicit def module(implicit G: Group[ConstantStep]): Module[Double, ConstantStep] =
    Module.from((d, cs) => ConstantStep(cs.value * d, cs.time))
}

class ConstantStepGroup(
    alpha: ConstantStep.Alpha,
    eps: ConstantStep.Epsilon
) extends Group[ConstantStep] {
  override val zero: ConstantStep = ConstantStep.zero

  override def isNonZero(cs: ConstantStep) = (cs.value != 0L)

  override def negate(v: ConstantStep) = ConstantStep(-v.value, v.time)

  override def plus(l: ConstantStep, r: ConstantStep) = {
    val (a, b, t) =
      if (l.time < r.time)
        (l.decayTo(r.time, alpha, eps), r, r.time)
      else
        (l, r.decayTo(l.time, alpha, eps), l.time)

    ConstantStep(a.value + b.value, t)
  }

  /**
    * Returns the value if the timestamp is less than the time of the
    * supplied ConstantStep instance.
    */
  def valueAsOf(v: ConstantStep, time: Time): Double =
    v.decayTo(time, alpha, eps).value

  /**
    * This assigns the reward at the current time, which forces the
    * timestamp forward.
    *
    * If you didn't bump the time you could do this:
    *
    * modern + (reward * (alpha / (1.0 - alpha)))
    *
    * And force the alpha to be less than one.
    *
    */
  def reward(v: ConstantStep, reward: Double, time: Time): ConstantStep = {
    val newTime = time.tick
    val updatedV = v.decayTo(newTime, alpha, eps).value
    ConstantStep(
      updatedV + alpha * reward,
      newTime
    )
  }
}
