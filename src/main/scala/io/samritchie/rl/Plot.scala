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
  * WELLLLL maybe evilplot is better!
  * https://cibotech.github.io/evilplot/plot-catalog.html
  *
  *
  */
package io.samritchie.rl

import com.cibo.evilplot.colors.{HSL, HTMLNamedColors}
import com.cibo.evilplot.displayPlot
import com.cibo.evilplot.numeric.{Bounds, Point}
import com.cibo.evilplot.plot.{FunctionPlot, LinePlot, Overlay}
import com.cibo.evilplot.plot.aesthetics.DefaultTheme

object Main {
  import DefaultTheme._
  import HTMLNamedColors._

  def lineChart(): Unit = {
    val data = Seq.tabulate(100) { i =>
      Point(i.toDouble, scala.util.Random.nextDouble())
    }
    displayPlot {
      LinePlot.series(data, "Line graph", HSL(210, 100, 56)).
        xAxis().yAxis().frame().
        xLabel("x").yLabel("y").render()
    }
  }

  def polyPlot(): Unit = {
    val x = Overlay(
      FunctionPlot.series(x => x * x, "y = x^2",
        HTMLNamedColors.dodgerBlue, xbounds = Some(Bounds(-1, 1))),
      FunctionPlot.series(x => math.pow(x, 3), "y = x^3",
        HTMLNamedColors.crimson, xbounds = Some(Bounds(-1, 1))),
      FunctionPlot.series(x => math.pow(x, 4), "y = x^4",
        HTMLNamedColors.green, xbounds = Some(Bounds(-1, 1)))
    ).title("A bunch of polynomials.")
      .overlayLegend()
      .standard()
      .render()
    displayPlot(x)
  }

  def main(items: Array[String]): Unit = {
    lineChart()
    polyPlot()
  }
}
