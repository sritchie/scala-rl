package com

/** Functional reinforcement learning in Scala.
  */
package object scalarl {

  /** Type alias for [[com.scalarl.rainier.Categorical]], which represents a finite discrete
    * probability distribution.
    */
  type Cat[+T] = rainier.Categorical[T]
}
