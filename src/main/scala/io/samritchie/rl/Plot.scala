/**
  * The good stuff. Plotting charts. Options were Plotly and
  * Breeze-Viz... but then, those are both a little busted. So I
  * decided to go with Evilplot.
  *
  * https://cibotech.github.io/evilplot/plot-catalog.html
  */
package io.samritchie.rl

import com.cibo.evilplot.colors.{HSL, HTMLNamedColors}
import com.cibo.evilplot.displayPlot
import com.cibo.evilplot.numeric.{Bounds, Point}
import com.cibo.evilplot.plot.{FunctionPlot, LinePlot, Overlay}
import com.cibo.evilplot.plot.aesthetics.DefaultTheme

object Plot {
  import DefaultTheme._
  import HTMLNamedColors._

  // Example of a linechart, just testing it out.
  def lineChart(): Unit = {
    val data = Seq.tabulate(100) { i =>
      Point(i.toDouble, scala.util.Random.nextDouble())
    }
    displayPlot {
      LinePlot
        .series(data, "Line graph", HSL(210, 100, 56))
        .xAxis()
        .yAxis()
        .frame()
        .xLabel("x")
        .yLabel("y")
        .render()
    }
  }

  // test of a polynomail plot, again, just an example to work with.
  def polyPlot(): Unit = {
    val x = Overlay(
      FunctionPlot.series(x => x * x, "y = x^2", HTMLNamedColors.dodgerBlue, xbounds = Some(Bounds(-1, 1))),
      FunctionPlot
        .series(x => math.pow(x, 3), "y = x^3", HTMLNamedColors.crimson, xbounds = Some(Bounds(-1, 1))),
      FunctionPlot
        .series(x => math.pow(x, 4), "y = x^4", HTMLNamedColors.green, xbounds = Some(Bounds(-1, 1)))
    ).title("A bunch of polynomials.")
      .overlayLegend()
      .standard()
      .render()
    displayPlot(x)
  }

  def main(items: Array[String]): Unit = {
    Game.playAndPrintOnce(nRuns = 1, timeSteps = 10000)
    lineChart()
    polyPlot()
  }
}
