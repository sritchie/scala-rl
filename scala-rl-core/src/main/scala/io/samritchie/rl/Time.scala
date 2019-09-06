package io.samritchie.rl

case class Time(value: Long) extends AnyVal {
  def tick: Time = Time(value + 1)
}

object Time {
  val zero: Time = Time(0L)
}
