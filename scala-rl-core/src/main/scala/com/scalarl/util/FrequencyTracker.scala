package com.scalarl
package util

import com.twitter.algebird.Monoid

/** Aggregating thing that also keeps track of frequencies. The item will be paired with a zero if
  * this is the first time seeing it.
  */
case class FrequencyTracker[A, B](
    items: Vector[(A, Int)],
    frequencies: Map[B, Int],
    f: A => B
) {
  def :+(a: A): FrequencyTracker[A, B] = {
    val b = f(a)
    val newFrequencies = Util.mergeV(frequencies, b, 1)
    FrequencyTracker(items :+ ((a, newFrequencies(b) - 1)), newFrequencies, f)
  }
  def iterator: Iterator[(A, Int)] = items.iterator
  def reverseIterator: Iterator[(A, Int)] = items.reverse.iterator
}
object FrequencyTracker {
  def empty[A, B](f: A => B): FrequencyTracker[A, B] =
    FrequencyTracker(Vector.empty, Map.empty[B, Int], f)

  def pure[A, B](a: A, f: A => B): FrequencyTracker[A, B] = empty(f) :+ a

  def monoid[A, B](f: A => B): Monoid[FrequencyTracker[A, B]] =
    new Monoid[FrequencyTracker[A, B]] {
      val zero: FrequencyTracker[A, B] = FrequencyTracker.empty(f)
      def plus(
          l: FrequencyTracker[A, B],
          r: FrequencyTracker[A, B]
      ): FrequencyTracker[A, B] =
        FrequencyTracker(
          l.items ++ r.items,
          Monoid.plus[Map[B, Int]](l.frequencies, r.frequencies),
          l.f
        )
    }
}
