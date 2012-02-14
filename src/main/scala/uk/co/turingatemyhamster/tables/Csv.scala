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

trait CsvTabularParser extends Csv with TabularParser with QuotedCellsParser

class CsvTabularAstParser extends CsvTabularParser with TabularAstBuilder with QuotedCellsAstBuilder

trait CsvTabularRenderer extends Csv with QuotedCellsRenderer with TabularRenderer

class CsvTabularAstRenderer(val out: Appendable) extends CsvTabularRenderer with TabularAstRenderer with QuotedCellsAstRenderer