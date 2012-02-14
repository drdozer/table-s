package uk.co.turingatemyhamster.tables

/**
 * Tsv - tab-separated values.
 *
 * @author Matthew Pocock
 */

trait Tsv {
  def cellSep = "\t"

  def quote = "\""
}

trait TsvTabularParser extends TabularParser with Tsv

class TsvTabularAstParser extends TsvTabularParser with TabularAstParser

trait TsvTabularRenderer extends TabularRenderer with Tsv

class TsvTabularAstRenderer(val out: Appendable) extends TsvTabularRenderer with TabularAstRenderer
