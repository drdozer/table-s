package uk.co.turingatemyhamster.tables

import java.lang.Appendable
import util.parsing.combinator.RegexParsers
import util.matching.Regex

trait Tabular {

  type T_Cell
  type T_HeaderRow
  type T_BodyRow
  type T_Table

  def cellSep: String

}

trait QuotedCells extends Tabular {

  type T_QuotedCell <: T_Cell
  type T_UnquotedCell <: T_Cell

  def quote: String

}

trait TabularConstructors extends Tabular {

  def handle_headerRow(headers: Traversable[T_Cell]): T_HeaderRow

  def handle_bodyRow(cells: Traversable[T_Cell]): T_BodyRow

  def handle_table(header: Option[T_HeaderRow], rows: Traversable[T_BodyRow]): T_Table
}

trait QuotedCellsTabularConstructors extends TabularConstructors with QuotedCells {

  def handle_quotedCell(s: String): T_QuotedCell

  def handle_unquotedCell(s: String): T_UnquotedCell

}

trait TabularParser extends TabularConstructors with RegexParsers {

  def cell: Parser[T_Cell]

  lazy val cells: Parser[List[T_Cell]] = repsep(cell, cellSep)

  lazy val headerRow: Parser[T_HeaderRow] = cells ^^ handle_headerRow

  lazy val bodyRow: Parser[T_BodyRow] = cells ^^ handle_bodyRow

  lazy val withHeader: Parser[T_Table] = headerRow ~ bodyRow.* ^^ {
    case hr ~ brs => handle_table(Some(hr), brs)
  }

  lazy val withoutHeader: Parser[T_Table] = bodyRow.* ^^ {
    case brs => handle_table(None, brs)
  }


  implicit def regex(rg: (Regex, Int)): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val (r, g) = rg
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(matched.group(g),
                  in.drop(start + matched.end - offset))
        case None =>
          val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
          Failure("string matching regex `"+r+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

}

trait QuotedCellsTabularTabularParser extends TabularParser with QuotedCellsTabularConstructors {

  lazy val quotedStringWithoutQuotes: Parser[String] = (quote + "([" + quote + "]*)" + quote).r -> 1

  lazy val unquotedString: Parser[String] = ("[^" + cellSep + "]*").r

  lazy val quotedString: Parser[T_QuotedCell] = quotedStringWithoutQuotes ^^ handle_quotedCell

  lazy val rawString: Parser[T_UnquotedCell] = unquotedString ^^ handle_unquotedCell

  lazy val cell = quotedString | rawString

}

trait TabularDestructors extends Tabular {

  def decompose_headerRow(headers: T_HeaderRow): Traversable[T_Cell]

  def decompose_bodyRow(row: T_BodyRow): Traversable[T_Cell]

  def decompose_table(table: T_Table): (Option[T_HeaderRow], Traversable[T_BodyRow])

}

trait QuotedCellsTabularDestructors extends TabularDestructors with QuotedCells {

  def decompose_quotedCell(cell: T_QuotedCell): String

  def decompose_unquotedCell(cell: T_UnquotedCell): String

}

trait TabularRenderer extends TabularDestructors {

  private def nl() {
    out append "\n"
  }

  def out: Appendable

  def render_cell(cell: T_Cell)

  def render_cellSeparator() {
    out append cellSep
  }

  def render_cells(cells: Traversable[T_Cell]) {
    if (!cells.isEmpty) {
      val first = cells.head
      val rest = cells.tail

      render_cell(first)
      for (c <- rest) {
        render_cellSeparator()
        render_cell(c)
      }
    }

    nl
  }

  def render_headerRow(headers: T_HeaderRow) {
    val cells = decompose_headerRow(headers)

    render_cells(cells)
  }

  def render_bodyRow(row: T_BodyRow) {
    val cells = decompose_bodyRow(row)

    render_cells(cells)
  }

  def render_table(table: T_Table) {
    val (headers, body) = decompose_table(table)

    for (h <- headers) render_headerRow(h)
    for (r <- body) render_bodyRow(r)
  }
}

trait QuotedCellsTabularRenderer extends TabularRenderer with QuotedCellsTabularDestructors {

  def render_quotedCell(cell: T_QuotedCell) {
    val s = decompose_quotedCell(cell)

    out append quote append s append quote
  }

  def render_unquotedCell(cell: T_UnquotedCell) {
    val s = decompose_unquotedCell(cell)

    out append s
  }

}