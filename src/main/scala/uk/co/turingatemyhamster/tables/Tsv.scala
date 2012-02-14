package uk.co.turingatemyhamster.tables

/**
 * Tsv - tab-separated values.
 *
 * @author Matthew Pocock
 */

trait Tsv extends Tabular {
  def cellSep = "\t"
}

trait TsvTabularParser extends Tsv with TabularParser with CharSequenceCellsParser

class TsvTabularAstParser extends TsvTabularParser with TabularAstBuilder

trait TsvTabularRenderer extends Tsv with TabularRenderer

class TsvTabularAstRenderer(val out: Appendable)
  extends TsvTabularRenderer with TabularAstRenderer with CharSequenceCellsRenderer
