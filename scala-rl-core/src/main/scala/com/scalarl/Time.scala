package com.scalarl

/** A value class wrapper around Long that allows us to talk about time ticking and evolution in a
  * type-safe way.
  *
  * This class provides methods for incrementing time, comparing time values, and basic arithmetic
  * operations, while maintaining type safety through the AnyVal wrapper.
  */

case class Time(value: Long) extends AnyVal {
  def tick: Time = Time(value + 1)
  def -(r: Time) = value - r.value
  def +(r: Time) = value + r.value
  def <=(r: Time) = value <= r.value
  def <(r: Time) = value < r.value
  def compareTo(r: Time) = value.compareTo(r.value)
}

object Time {
  val Min: Time = Time(Long.MinValue)
  val Max: Time = Time(Long.MaxValue)
  val Zero: Time = Time(0L)
}
