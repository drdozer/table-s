package uk.co.turingatemyhamster.tables

import java.lang.Appendable
import util.parsing.combinator.RegexParsers
import util.matching.Regex

trait Cellular {

  type T_Cell


  def cellSep: String
  def newline: String = "\n"
}

trait Tabular extends Cellular {

  type T_HeaderRow
  type T_BodyRow
  type T_Table

}


trait TabularConstructors extends Tabular {

  def handle_headerRow(headers: Iterable[T_Cell]): T_HeaderRow

  def handle_bodyRow(cells: Iterable[T_Cell]): T_BodyRow

  def handle_table(header: Option[T_HeaderRow], rows: Iterable[T_BodyRow]): T_Table
}

trait EnhancedRegexParsers extends RegexParsers {

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

trait TabularParser extends TabularConstructors with EnhancedRegexParsers {

  override def skipWhitespace = false

  def cell: Parser[T_Cell]

  val nl: Parser[String] = newline

  lazy val cells: Parser[List[T_Cell]] = (repsep(cell, cellSep) <~ nl)

  lazy val headerRow: Parser[T_HeaderRow] = cells ^^ handle_headerRow

  lazy val bodyRow: Parser[T_BodyRow] = cells ^^ handle_bodyRow

  lazy val body: Parser[List[T_BodyRow]] = bodyRow.* <~ newline.?

  lazy val withHeader: Parser[T_Table] = headerRow ~ body ^^ {
    case hr ~ brs => handle_table(Some(hr), brs)
  }

  lazy val withoutHeader: Parser[T_Table] = body ^^ {
    case brs => handle_table(None, brs)
  }


}

trait TabularDestructors extends Tabular {

  def decompose_headerRow(headers: T_HeaderRow): Iterable[T_Cell]

  def decompose_bodyRow(row: T_BodyRow): Iterable[T_Cell]

  def decompose_table(table: T_Table): (Option[T_HeaderRow], Iterable[T_BodyRow])

}

trait Renderer {

  def out: Appendable

}

trait CellRenderer extends Cellular with Renderer {

  def render_cell(cell: T_Cell)

}

trait TabularRenderer extends CellRenderer with TabularDestructors {

  def nl() {
    out append newline
  }

  def render_cellSeparator() {
    out append cellSep
  }

  def render_cells(cells: Iterable[T_Cell]) {
    if (!cells.isEmpty) {
      val first = cells.head
      val rest = cells.tail

      render_cell(first)
      for (c <- rest) {
        render_cellSeparator()
        render_cell(c)
      }
    }

    nl()
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
