package uk.co.turingatemyhamster.tables

/**
 * Created by IntelliJ IDEA.
 * User: nmrp3
 * Date: 14/02/12
 * Time: 18:41
 * To change this template use File | Settings | File Templates.
 */

trait CharSequenceCells extends Cellular {

  type T_Cell = CharSequence
  
}

trait CharSequenceCellsConstructors extends CharSequenceCells

trait CharSequenceCellsParser extends CharSequenceCellsConstructors with EnhancedRegexParsers {
  
  lazy val cell: Parser[CharSequence] = ("[^" + cellSep + "]*").r
  
}

trait CharSequenceCellsDestructors extends CharSequenceCells

trait CharSequenceCellsRenderer extends CellRenderer with CharSequenceCellsDestructors {
  def render_cell(cell: CharSequence) = out append cell
}