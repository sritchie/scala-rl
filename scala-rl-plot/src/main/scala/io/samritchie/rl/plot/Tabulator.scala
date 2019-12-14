package io.samritchie.rl
package plot

/**
  * Shamelessly copied from:
  * http://stackoverflow.com/a/7542476
  *
  * I modified this slightly to take Iterable instances instead of Seq
  * instances; this still needs to be updated to handle more formatting options.
  *
  * I'm using this to print tables for Gridworld.
  */
object Tabulator {

  def csv(table: Iterable[Iterable[Any]]): String = table.map(_.mkString(",")).mkString("\n")

  def format(table: Iterable[Iterable[Any]]): String =
    if (table.isEmpty) ""
    else {
      val sizes =
        for (row <- table)
          yield (for (cell <- row)
            yield
              if (cell == null) 0
              else cell.toString.length)
      val colSizes = for (col <- sizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
    }

  def formatRows(rowSeparator: String, rows: Iterable[String]): String =
    (rowSeparator :: rows.toList ::: rowSeparator :: List(
      )).mkString("\n")

  def formatRow(row: Iterable[Any], colSizes: Iterable[Int]): String = {
    val cells =
      (for ((item, size) <- row.zip(colSizes))
        yield
          if (size == 0) ""
          else ("%" + size.toString + "s").format(item))
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: Iterable[Int]): String =
    colSizes
      .map {
        "-" * _
      }
      .mkString("+", "+", "+")
}
