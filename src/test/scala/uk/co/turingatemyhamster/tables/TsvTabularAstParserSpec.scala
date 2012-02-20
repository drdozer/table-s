package uk.co.turingatemyhamster.tables

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class TsvTabularAstParserSpec extends Specification with ParserMatchers {

  val parsers = new TsvTabularAstParser
  import parsers._
  
  "the cell parser" should {
    "parse all of a string" in {
      cell must succeedOn("hi mum").withResult("hi mum")
    }
    
    "parse up to tab" in {
      cell must succeedOn ("hi\tmum").partially.withResult("hi")
    }
    
    "parse up to newline" in {
      cell must succeedOn  ("hi\nmum").partially.withResult("hi")
    }
  }
  
  "the cells parser" should {
    "parse one item" in {
      cells must succeedOn("apple\n").withResult(("apple": CharSequence) :: Nil)
    }
    
    "parse two items" in {
      cells must succeedOn("apple\tbanana\n").withResult(("apple": CharSequence) :: "banana" :: Nil)
    }
    
    "parse one item but not go onto the next line" in {
      cells must succeedOn ("apple\nbanana\n").partially.withResult(("apple": CharSequence) :: Nil)
    }
  }
  
}
