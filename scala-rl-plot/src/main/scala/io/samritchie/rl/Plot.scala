/**
  * The good stuff. Plotting charts. Options were Plotly and
  * Breeze-Viz... but then, those are both a little busted. So I
  * decided to go with Evilplot.
  *
  * https://cibotech.github.io/evilplot/plot-catalog.html
  *
  * Here's a great example of the kinds of things we can do with this
  * plotting library:
  *
  * https://www.cibotechnologies.com/about/blog/scalastan-and-evilplot-bayesian-statistics-meets-combinator-based-visualization/
  */
package io.samritchie.rl

import com.cibo.evilplot.colors.{HTMLNamedColors, RGB}
import com.cibo.evilplot.displayPlot
import com.cibo.evilplot.numeric.{Bounds, Point}
import com.cibo.evilplot.plot.{FunctionPlot, LinePlot, Overlay}
import com.cibo.evilplot.plot.aesthetics.DefaultTheme

object Plot {
  import DefaultTheme._

  // Example of a linechart, just testing it out.
  def lineChartSeq(pointSeq: (Seq[Double], String)*): Unit =
    lineChart(
      pointSeq.map {
        case (points, title) =>
          (points.toList.zipWithIndex.map { case (a, i) => Point(i, a) }, title)
      }
    )

  def lineChart(data: Seq[(Seq[Point], String)]): Unit =
    displayPlot {
      Overlay(
        data.map {
          case (points, title) =>
            LinePlot.series(points, title, RGB.random)
        }: _*
      ).xAxis()
        .yAxis()
        .frame()
        .xLabel("x")
        .yLabel("y")
        .title("Yo!")
        .overlayLegend()
        .render()
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
    lineChart(Seq(Seq.tabulate(100) { i =>
      Point(i.toDouble, scala.util.Random.nextDouble())
    } -> "Title."))
    polyPlot()
  }
}
