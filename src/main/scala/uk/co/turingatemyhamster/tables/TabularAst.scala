package uk.co.turingatemyhamster.tables

/**
 * Created by IntelliJ IDEA.
 * User: nmrp3
 * Date: 01/02/12
 * Time: 15:07
 * To change this template use File | Settings | File Templates.
 */

trait TabularAst extends Tabular with QuotedCells {
  type T_Cell = Cell
  type T_QuotedCell = QuotedCell
  type T_UnquotedCell = UnquotedCell

  type T_HeaderRow = HeaderRow
  type T_BodyRow = BodyRow
  type T_Table = Table
}

sealed trait Cell

object Cell {
  def unapply(c: Cell): Option[String] = c match {
    case UnquotedCell(s) => Some(s)
    case QuotedCell(s) => Some(s)
  }
}

case class UnquotedCell(s: String) extends Cell
case class QuotedCell(s: String) extends Cell

case class HeaderRow(headers: Iterable[Cell])
case class BodyRow(cells: Iterable[Cell])
case class Table(headers: Option[HeaderRow], rows: Iterable[BodyRow])

trait TabularAstConstructors extends TabularAst with QuotedCellsTabularConstructors

trait TabularAstBuilder extends TabularAstConstructors {

  def handle_quotedCell(s: String) =
    QuotedCell(s)

  def handle_unquotedCell(s: String) =
    UnquotedCell(s)

  def handle_headerRow(headers: Iterable[TabularAstBuilder#T_Cell]) =
    HeaderRow(headers)

  def handle_bodyRow(cells: Iterable[TabularAstBuilder#T_Cell]) =
    BodyRow(cells)

  def handle_table(header: Option[TabularAstBuilder#T_HeaderRow], rows: Iterable[TabularAstBuilder#T_BodyRow]) =
    Table(header, rows)
}

trait TabularAstParser extends TabularAstBuilder with QuotedCellsTabularTabularParser




trait TabularAstDestructors extends TabularAst with QuotedCellsTabularDestructors {
  def decompose_headerRow(headers: TabularAstDestructors#T_HeaderRow) = headers.headers

  def decompose_bodyRow(row: TabularAstDestructors#T_BodyRow) = row.cells

  def decompose_table(table: TabularAstDestructors#T_Table) = (table.headers, table.rows)

  def decompose_quotedCell(cell: TabularAstDestructors#T_QuotedCell) = cell.s

  def decompose_unquotedCell(cell: TabularAstDestructors#T_UnquotedCell) = cell.s
}

trait TabularAstRenderer extends TabularAstDestructors with QuotedCellsTabularRenderer {
  def render_cell(cell: T_Cell) {
    cell match {
      case q : QuotedCell => render_quotedCell(q)
      case u : UnquotedCell => render_unquotedCell(u)
    }
  }
}