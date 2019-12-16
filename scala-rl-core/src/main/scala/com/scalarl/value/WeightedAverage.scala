package com.scalarl
package value

import _root_.algebra.CommutativeGroup
import com.twitter.algebird.Group
import com.scalarl.algebra.Weight

/**
  This is of course extremely similar to the averaged value implementation in
  Algebird... it just keeps track of a numerator AND denominator
  */
case class WeightedAverage(weightSum: Weight, value: Double) {

  /**
    * Returns a copy of this instance with a negative value. Note that
    *
    * {{{
    * a + -b == a - b
    * }}}
    */
  def unary_- : WeightedAverage = copy(value = -value)

  /**
    * Averages this instance with the *opposite* of the supplied
    * [[WeightedAverage]] instance, effectively subtracting out that
    * instance's contribution to the mean.
    *
    * @param r the instance to subtract
    * @return an instance with `r`'s stream subtracted out
    */
  def -(r: WeightedAverage): WeightedAverage =
    WeightedAverageGroup.minus(this, r)

  /**
    * Averages this instance with another [[WeightedAverage]] instance.
    * @param r the other instance
    * @return an instance representing the mean of this instance and `r`.
    */
  def +(r: WeightedAverage): WeightedAverage =
    WeightedAverageGroup.plus(this, r)

  /**
    * Returns a new instance that averages `that` into this instance.
    *
    * @param that value to average into this instance
    * @return an instance representing the mean of this instance and `that`.
    */
  def +(that: Double): WeightedAverage = plus(that, Weight.One)

  def plus(that: Double, weight: Weight): WeightedAverage =
    WeightedAverage(
      weightSum + weight,
      WeightedAverageGroup.getCombinedMean(weightSum.w, value, weight.w, that)
    )
}

object WeightedAverage {
  implicit val group: Group[WeightedAverage] = WeightedAverageGroup
}

/**
  * [[Group]] implementation for [[WeightedAverage]].
  *
  * @define object `WeightedAverage`
  */
object WeightedAverageGroup extends Group[WeightedAverage] with CommutativeGroup[WeightedAverage] {

  /**
    * When combining averages, if the counts sizes are too close we
    * should use a different algorithm.  This constant defines how
    * close the ratio of the smaller to the total count can be:
    */
  private val STABILITY_CONSTANT = 0.1

  /**
    * Given two streams of doubles (n, an) and (k, ak) of form (count,
    * mean), calculates the mean of the combined stream.
    *
    * Uses a more stable online algorithm which should be suitable for
    * large numbers of records similar to:
    * http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
    */
  private[scalarl] def getCombinedMean(n: Double, an: Double, k: Double, ak: Double): Double =
    if (n < k) getCombinedMean(k, ak, n, an)
    else
      (n + k) match {
        case 0.0                         => 0.0
        case newCount if (newCount == n) => an
        case newCount =>
          val scaling = k / newCount
          // a_n + (a_k - a_n)*(k/(n+k)) is only stable if n is not approximately k
          if (scaling < STABILITY_CONSTANT) (an + (ak - an) * scaling)
          else (n * an + k * ak) / newCount
      }

  override val zero: WeightedAverage = WeightedAverage(Weight.Zero, 0.0)

  override def isNonZero(av: WeightedAverage): Boolean = (av.value != 0L)

  override def negate(av: WeightedAverage): WeightedAverage = -av

  /**
    * Optimized implementation of [[plus]]. Uses internal mutation to
    * combine the supplied [[WeightedAverage]] instances without creating
    * intermediate objects.
    */
  override def sumOption(iter: TraversableOnce[WeightedAverage]): Option[WeightedAverage] =
    if (iter.isEmpty) None
    else {
      var weightSum = 0.0
      var average = 0.0
      iter.foreach {
        case WeightedAverage(Weight(w), v) =>
          average = getCombinedMean(weightSum, average, w, v)
          weightSum += w
      }
      Some(WeightedAverage(Weight(weightSum), average))
    }

  /**
    * @see [[WeightedAverage!.+(r:*]] for the implementation.
    */
  override def plus(l: WeightedAverage, r: WeightedAverage): WeightedAverage = {
    val n = l.weightSum
    val k = r.weightSum
    val newAve = getCombinedMean(n.w, l.value, k.w, r.value)
    WeightedAverage(n + k, newAve)
  }
}
