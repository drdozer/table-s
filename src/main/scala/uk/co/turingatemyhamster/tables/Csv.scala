package uk.co.turingatemyhamster.tables


/**
 * CSV - comma-separated values.
 *
 * @author Matthew Pocock
 */

trait Csv extends QuotedCells {
  def cellSep = ","

  def quote = "\""
}

trait CsvTabularParser extends QuotedCellsTabularParser with Csv

class CsvTabularAstParser extends CsvTabularParser with TabularAstParser

trait CsvTabularRenderer extends QuotedCellsTabularRenderer with Csv

class CsvTabularAstRenderer(val out: Appendable) extends CsvTabularRenderer with TabularAstRenderer