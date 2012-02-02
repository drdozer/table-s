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
    "parses an empty quoted string" in {
      quotedString must succeedOn("\"\"").withResult(QuotedCell(""))
    }

    "parse all of a quoted string" in {
      quotedString must succeedOn(""""quoted"""").withResult(QuotedCell("quoted"))
    }

    "parses quoted string prefix followed by separater" in {
      quotedString must succeedOn(""""quoted",bob""").partially.withResult(QuotedCell("quoted"))
    }

    "parses quoted string prefix followed by newlines" in {
      quotedString must succeedOn(""""quoted"\nbob""").partially.withResult(QuotedCell("quoted"))
    }

    "parse quoted string containing quote-escaped quote" in {
      quotedString must succeedOn("\"prefix\"\"suffix\"").withResult(QuotedCell("prefix\"suffix"))
    }

    "fail on an unquoted string" in {
      quotedString must failOn("unquoted")
    }
  }
  
  "the unquoted cell parser" should {
    "parse an empty string" in {
      unquotedString must succeedOn("").withResult(UnquotedCell(""))
    }

    "parse all of an unquoted string" in {
      unquotedString must succeedOn("""unquoted""").withResult(UnquotedCell("unquoted"))
    }

    "parse unquoted string prefix followed by separater" in {
      unquotedString must succeedOn("unquoted,other stuff").partially.withResult(UnquotedCell("unquoted"))
    }

    "parse unquoted string prefix reading through tab" in{
      unquotedString must succeedOn("unquoted\tother stuff").partially.withResult(UnquotedCell("unquoted\tother stuff"))
    }

    "parse unquoted string prefix" in {
      unquotedString must succeedOn("unquoted\nother stuff").partially.withResult(UnquotedCell("unquoted"))
    }

    "fail on a quoted string" in {
      unquotedString must failOn(""""quoted"""")
    }
  }

  "the cell parser" should {
    "parse all of a quoted string" in {
      cell must succeedOn(""""quoted"""").withResult(QuotedCell("quoted") : Cell)
    }

    "parse all of an unquoted string" in {
      cell must succeedOn("""unquoted""").withResult(UnquotedCell("unquoted") : Cell)
    }
  }

  "the cells parser" should {
    "parse one item" in {
      cells must succeedOn("apple\n").withResult((UnquotedCell("apple"): Cell) :: Nil)
    }

    "parse two items" in {
      cells must succeedOn("apple,banana\n").withResult((UnquotedCell("apple"): Cell)::UnquotedCell("banana")::Nil)
    }
  }

  "the bodyrow parser" should {
    "parse the first example row" in {
      bodyRow must succeedOn(
"""... rides again,Idiom,,,,"ride, ",,"used to indicate that someone or something has reappeared, especially unexpectedly and with renewed vigour",,,
""").withResult(BodyRow(List(
        UnquotedCell("... rides again"),
        UnquotedCell("Idiom"),
        UnquotedCell(""),
        UnquotedCell(""),
        UnquotedCell(""),
        QuotedCell("ride, "),
        UnquotedCell(""),
        QuotedCell("used to indicate that someone or something has reappeared, especially unexpectedly and with renewed vigour"),
        UnquotedCell(""),
        UnquotedCell(""),
        UnquotedCell(""))))
    }

    "parse the second example row" in {
      bodyRow must succeedOn(
"""... rides again,Idiom,,,,"again, ride",,,,,
""").withResult(BodyRow(List(
        UnquotedCell("... rides again"),
        UnquotedCell("Idiom"),
        UnquotedCell(""),
        UnquotedCell(""),
        UnquotedCell(""),
        QuotedCell("again, ride"),
        UnquotedCell(""),
        UnquotedCell(""),
        UnquotedCell(""),
        UnquotedCell(""),
        UnquotedCell("")))
      )
    }
  }

  "the withHeader parser" should {
    "parse a small table" in {
      withHeader must succeedOn(
"""PHRASE,CATEGORY 1,CATEGORY 2,CATEGORY 3,ADLT,KEYWORD,THEMES,DEFINITION,SOURCE/ARTIST/PERSON,THESAURUS,COUNTRY
... a pop,Idiom,,,,pop,"cost, price",costing a specified amount per item,,,
... a treat,Idiom,,,,treat,"do well, satisfactorily",used to indicate that someone or something does something specified very well or satisfactorily,,,
... and all,Idiom,,,,all,additional,used emphasise something additional that is being referred to,,,
... as sin,Idiom,,,,sin,undesirable,"having a particular undesirable quality to high degree e.g. guilty as sin, lazy as sin, etc",,,
... as we know it,Idiom,,,,"know, ","familiar, customary",as is familiar or customary in the present,,,
... from hell,Idiom,,,,"hell, ","unpleasant, troublesome",an extremely unpleasant or troublesome instance or example of something,,,
... going on ...,Idiom,,,,"going, ","attitude, behaviour",used to suggest someone's behaviour or attitudes are those of someone older or younger than their age e.g. He is 21 going on 40,,,
... in residence,Idiom,,,,"residence, ",,a person with a specified occupation (especially an artist or writer) paid to work for a time in a college or other institution,,,
... of the clock ,Idiom,,,,clock,,,,,
... or no ...,Idiom,,,,"no, ",,"regardless of the person, thing, or quality specified",,,
... permitting,Idiom,,,,"permitting, ",,if the specified thing does not prevent you from doing something,,,
... rides again,Idiom,,,,"ride, ",,"used to indicate that someone or something has reappeared, especially unexpectedly and with renewed vigour",,,
... rides again,Idiom,,,,"again, ride",,,,,
"... rules, ok?",Idiom,,,,"rule, ",,used to express your enthusiasm for a particular person or thing,,,
... strikes again,Idiom,,,,"strike, ",,something or someone acts again or reappears in characteristic fashion and with notable effect,,,
... up a storm,Idiom,,,,"storm, ",,perform a particular action with great enthusiasm and energy,,,
... your guts out,Idiom,,,,"gut, ",,perform a specified action as hard or as fully as possible,,,
... your head off,Idiom,,,,"head, ",,"laugh, talk, shout, etc with complete lack of restraint or without stopping",,,
... yourself silly,Idiom,,,,"silly, ",,be unable to act rationally because of doing something to excess,,,
""")
    }
  }
}