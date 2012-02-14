package uk.co.turingatemyhamster.tables

/**
 * Created by IntelliJ IDEA.
 * User: nmrp3
 * Date: 01/02/12
 * Time: 15:07
 * To change this template use File | Settings | File Templates.
 */

case class HeaderRow[C](headers: Iterable[C])
case class BodyRow[C](cells: Iterable[C])
case class Table[C](headers: Option[HeaderRow[C]], rows: Iterable[BodyRow[C]])



trait TabularAst extends Tabular {

  type T_HeaderRow = HeaderRow[T_Cell]
  type T_BodyRow = BodyRow[T_Cell]
  type T_Table = Table[T_Cell]

}


trait TabularAstConstructors extends TabularAst with TabularConstructors

trait TabularAstBuilder extends TabularAstConstructors {

  def handle_headerRow(headers: Iterable[T_Cell]) =
    HeaderRow(headers)

  def handle_bodyRow(cells: Iterable[T_Cell]) =
    BodyRow(cells)

  def handle_table(header: Option[T_HeaderRow], rows: Iterable[T_BodyRow]) =
    Table[T_Cell](header, rows)
}


trait TabularAstDestructors extends TabularAst with TabularDestructors {

  def decompose_headerRow(headers: T_HeaderRow) = headers.headers

  def decompose_bodyRow(row: T_BodyRow) = row.cells

  def decompose_table(table: T_Table) = (table.headers, table.rows)

}

trait TabularAstRenderer extends TabularAstDestructors with TabularRenderer
