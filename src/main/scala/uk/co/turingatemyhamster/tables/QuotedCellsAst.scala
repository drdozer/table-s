package uk.co.turingatemyhamster.tables


sealed trait Cell

object Cell {
  def unapply(c: Cell): Option[String] = c match {
    case UnquotedCell(s) => Some(s)
    case QuotedCell(s) => Some(s)
  }
}

case class UnquotedCell(s: String) extends Cell
case class QuotedCell(s: String) extends Cell

trait QuotedCellsAst extends QuotedCells {

  type T_Cell = Cell
  type T_QuotedCell = QuotedCell
  type T_UnquotedCell = UnquotedCell

}

trait QuotedCellsAstConstructors extends QuotedCellsAst with QuotedCellsConstructors

trait QuotedCellsAstBuilder extends QuotedCellsAstConstructors {
  def handle_quotedCell(s: String) = QuotedCell(s)

  def handle_unquotedCell(s: String) = UnquotedCell(s)
}

trait QuotedCellsAstDestructors extends QuotedCellsAst with QuotedCellsDestructors {

  def decompose_quotedCell(cell: T_QuotedCell) = cell.s

  def decompose_unquotedCell(cell: T_UnquotedCell) = cell.s

}

trait QuotedCellsAstRenderer extends QuotedCellsAstDestructors with QuotedCellsRenderer {

  def render_cell(cell: T_Cell) {
    cell match {
      case q : QuotedCell => render_quotedCell(q)
      case u : UnquotedCell => render_unquotedCell(u)
    }
  }

}