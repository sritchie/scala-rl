package com.scalarl

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
