/**
  Gambler's Problem! Chapter 4 again; this generates Figure 4.3.
  */
package io.samritchie.rl
package world

import io.samritchie.rl.util.Grid
import Grid.{Bounds, Position}

object GamblersProblem {
  case class Config(bounds: Bounds) {
    def build(start: Position): GamblersProblem = ???
  }
}

/**
  This is going to use a coinflip internally. Gotta read more about what the
  hell is going on, but the key is that we have 100 possible states... for the
  value function.
  */
case class GamblersProblem(grid: Grid) {}
