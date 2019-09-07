package io.samritchie.rl
package util

import com.twitter.algebird.Monoid

/**
  * Exponential recency-weighted average. This is similar to a
  * weighted average, but instead of weighting by the count, it uses a
  * constant weighting factor.
  */
case class ConstantStep(value: Double, time: Long) extends Ordered[ConstantStep] {
  import ConstantStep.{Alpha, Epsilon}

  def compare(that: ConstantStep): Int =
    time.compareTo(that.time) match {
      case 0     => value.compare(that.value)
      case other => other
    }

  def decayTo(t2: Long, alpha: Alpha, eps: Epsilon): ConstantStep =
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

  val zero: ConstantStep = ConstantStep(0.0, Long.MinValue)

  def build[T](value: T)(implicit num: Numeric[T]): ConstantStep =
    build(value, Long.MinValue)

  def build[T](value: T, time: Long)(implicit num: Numeric[T]): ConstantStep =
    ConstantStep(num.toDouble(value), time)

  def monoid(alpha: Alpha, eps: Epsilon): Monoid[ConstantStep] =
    new ConstantStepMonoid(alpha, eps)
}

case class ConstantStepMonoid(
    alpha: ConstantStep.Alpha,
    eps: ConstantStep.Epsilon
) extends Monoid[ConstantStep] {
  override val zero: ConstantStep = ConstantStep.zero

  override def isNonZero(cs: ConstantStep) = (cs.value != 0L)

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
  def valueAsOf(v: ConstantStep, time: Long): Double =
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
  def reward(v: ConstantStep, reward: Double, time: Long): ConstantStep = {
    val modern = v.decayTo(time, alpha, eps).value
    ConstantStep(
      modern + alpha * (reward - modern),
      time + 1
    )
  }
}
