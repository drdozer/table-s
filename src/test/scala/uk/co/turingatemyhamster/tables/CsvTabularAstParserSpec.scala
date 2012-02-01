package uk.co.turingatemyhamster.tables

import org.specs2.mutable.Specification
import org.specs2.matcher.ParserMatchers
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class CsvTabularAstParserSpec extends Specification with ParserMatchers {
  val parsers = new CsvTabularAstParser
  import parsers._

  "the quoted cell parser" should {
    "parse all of a quoted string" in {
      quotedString
    }
  }
}