package uk.co.turingatemyhamster

import java.io.Reader


package object tables {

  def parseCsv(in: Reader, requireHeaders: Boolean = true): Table[Cell] = {

    val csv = new CsvTabularAstParser
    import csv._

    val p = if(requireHeaders) withHeader else withoutHeader

    parseAll(p, in) match {
      case Success(t, _) => t
      case NoSuccess(msg, input) => sys.error("Unable to parse input: " + msg + " at " + input.pos);
    }

  }

  def parse(in: Reader,
            requireHeaders: Boolean = true,
            parser: TabularParser = new CsvTabularAstParser): parser.T_Table =
  {
    import parser._

    val p = if(requireHeaders) withHeader else withoutHeader

    parseAll(p, in) match {
      case Success(t, _) => t
      case NoSuccess(msg, input) => sys.error("Unable to parse input: " + msg + " at " + input.pos);
    }

  }
}