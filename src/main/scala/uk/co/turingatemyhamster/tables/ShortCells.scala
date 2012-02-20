package uk.co.turingatemyhamster.tables

/**
 * Created by IntelliJ IDEA.
 * User: nmrp3
 * Date: 14/02/12
 * Time: 22:59
 * To change this template use File | Settings | File Templates.
 */

trait ShortCells extends Cellular {

  type T_Cell = Short

}

trait ShortCellsConstructors extends ShortCells

trait ShortCellsParser extends ShortCellsConstructors with EnhancedRegexParsers {

  lazy val cell: Parser[Short] = """\d+""".r ^^ (_.toShort)

}

trait ShortCellsDestructors extends ShortCells

trait ShortCellsRenderer extends ShortCellsDestructors with CellRenderer {
  def render_cell(cell: Short) = out append cell.toString
}