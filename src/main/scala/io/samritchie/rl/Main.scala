package io.samritchie.rl

import breeze.linalg._
import breeze.plot._

/**
  * The good stuff. Plotting...
  *
  * https://zwild.github.io/posts/plotly-examples-for-scala/
  *
  * Visualization is important for data science and machine
  * learning. Breeze provide breeze-viz for this. However it has only
  * 4 kinds of plots -- line, scatter, histogram and image.
  *
  * Wow, plotly looks way better here...
  *
  * https://zwild.github.io/posts/plotly-examples-for-scala/
  *
  * Goal is still just to get some shit out.
  *
  *
  *
  * WELLLLL maybe evilplot is better!
  * https://cibotech.github.io/evilplot/plot-catalog.html
  *
  *
  */
object Main {
  def chartExample(): Unit = {
    val f = Figure()
    val p = f.subplot(0)
    val x = linspace(0.0, 1.0)
    p += plot(x, x.map(i => Math.pow(i, 2.0)))
    p += plot(x, x.map(i => Math.pow(i, 3.0)), '.')
    p.xlabel = "x axis"
    p.ylabel = "y axis"

    f.saveas("images/lines.png") // save current figure as a .png, eps and pdf also supported
  }

  def main(items: Array[String]): Unit = {
    chartExample()
  }
}
