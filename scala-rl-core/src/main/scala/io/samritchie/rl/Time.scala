package io.samritchie.rl

case class Time(value: Long) extends AnyVal {
  def tick: Time = Time(value + 1)
}

object Time {
  val min: Time = Time(Long.MinValue)
  val max: Time = Time(Long.MaxValue)
  val zero: Time = Time(0L)
}
