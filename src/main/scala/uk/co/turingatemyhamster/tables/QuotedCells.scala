package uk.co.turingatemyhamster.tables

/**
 * Created by IntelliJ IDEA.
 * User: nmrp3
 * Date: 14/02/12
 * Time: 18:38
 * To change this template use File | Settings | File Templates.
 */


trait QuotedCells extends Cellular {

  type T_QuotedCell <: T_Cell
  type T_UnquotedCell <: T_Cell

  def quote: String

}

trait QuotedCellsConstructors extends QuotedCells {

  def handle_quotedCell(s: String): T_QuotedCell

  def handle_unquotedCell(s: String): T_UnquotedCell

}

trait QuotedCellsParser extends QuotedCellsConstructors with EnhancedRegexParsers {

  lazy val quotedStringWithoutQuotes: Parser[String] = (quote + "(([^" + quote + newline + "]|(" + quote + quote + "))*)" + quote).r -> 1 ^^
    (_.replace(quote + quote, quote))

  lazy val unquotedStringAsIs: Parser[String] = ("[^" + cellSep + quote + newline + "]*").r

  lazy val quotedString: Parser[T_QuotedCell] = quotedStringWithoutQuotes ^^ handle_quotedCell

  lazy val unquotedString: Parser[T_UnquotedCell] = unquotedStringAsIs ^^ handle_unquotedCell

  lazy val cell = quotedString | unquotedString

}

trait QuotedCellsDestructors extends QuotedCells {

  def decompose_quotedCell(cell: T_QuotedCell): String

  def decompose_unquotedCell(cell: T_UnquotedCell): String

}

trait QuotedCellsRenderer extends CellRenderer with QuotedCellsDestructors {

  def render_quotedCell(cell: T_QuotedCell) {
    val s = decompose_quotedCell(cell)

    out append quote append s append quote
  }

  def render_unquotedCell(cell: T_UnquotedCell) {
    val s = decompose_unquotedCell(cell)

    out append s
  }

}