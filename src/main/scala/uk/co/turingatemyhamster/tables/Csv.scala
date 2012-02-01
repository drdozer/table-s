package uk.co.turingatemyhamster.tables


/**
 * Created by IntelliJ IDEA.
 * User: nmrp3
 * Date: 01/02/12
 * Time: 15:03
 * To change this template use File | Settings | File Templates.
 */

trait Csv extends QuotedCells {
  def cellSep = ","

  def quote = "\""
}

trait CsvTabularParser extends QuotedCellsTabularTabularParser with Csv

class CsvTabularAstParser extends CsvTabularParser with TabularAstParser

trait CsvTabularRenderer extends QuotedCellsTabularRenderer with Csv

class CsvTabularAstRenderer(val out: Appendable) extends CsvTabularRenderer with TabularAstRenderer