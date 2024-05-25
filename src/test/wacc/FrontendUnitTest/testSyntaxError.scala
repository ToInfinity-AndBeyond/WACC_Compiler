package frontendUnitTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.{Success, Failure}
import wacc.ast.ast._
import wacc.syntax.parser

/* Test syntax errors in invalid codes 
   Most cases are covered in integration test, 
   so this is for the early stage of parser implementation */

class UnitSyntaxTest extends AnyFlatSpec with Matchers {
  /* To run a single file from integration test, uncomment the code below */
  // val filePath = "../../tests/syntaxError/begins.wacc"
  
  // def readFile(file: String): String = 
  //     scala.io.Source.fromFile(file).getLines() mkString "\n"

  // "Test Single" should "try parsing" in {
  //   parser.parse(readFile(filePath)) shouldBe
  //     None
  // }

    
  "Program" should "be able to detect missing begin and end" in {
    parser.parse("begin") should
      matchPattern{ case Failure(_) => }
    parser.parse("end") should
      matchPattern{ case Failure(_) => }
    parser.parse("") should
      matchPattern{ case Failure(_) => }
  }

  "Comment" should "be able to detect missing comment" in {
    parser.parse("hello begin skip end") should
      matchPattern{ case Failure(_) => }
    parser.parse("begin \n hello \n skip \n end") should
      matchPattern{ case Failure(_) => }
    parser.parse("begin \n skip \n hello \n end") should
      matchPattern{ case Failure(_) => }
    parser.parse("begin \n skip \n end \n hello") should
      matchPattern{ case Failure(_) => }
    parser.parse("#hello \n begin #hello \n skip \n #hello \n end") should
      matchPattern{ case Success(Program(_, _)) => }
  }

  "Body" should "be able to detect missing body" in {
    parser.parse("begin end") should
      matchPattern{ case Failure(_) => }
    parser.parse("begin \n #hello \n end") should
      matchPattern{ case Failure(_) => }
    parser.parse("begin \n int fun() is return 0 \n # hello \n end") should
      matchPattern{ case Failure(_) => }
  }

  "Integer" should "be able to detect integer overflow" in {
    parser.parse("begin int x = 2147483647 end") should
      matchPattern{ case Success(Program(_, _)) => }
    parser.parse("begin int x = -2147483648 end") should
      matchPattern{ case Success(Program(_, _)) => }
    parser.parse("begin int x = -2147483649 end") should
      matchPattern{ case Failure(_) => }
    parser.parse("begin int x = 2147483648 end") should
      matchPattern{ case Failure(_) => }
  }

  "Character" should "be able to detect non-ASCII character" in {
    parser.parse("begin char a = 'a' end") should
      matchPattern{ case Success(Program(_, _)) => }
    parser.parse("begin char a = '0' end") should
      matchPattern{ case Success(Program(_, _)) => }
    parser.parse("begin char a = '\\n' end") should
      matchPattern{ case Success(Program(_, _)) => }
  }

  "Operator" should "be able to handle multiple operations" in {
    parser.parse("begin bool a = (true && (ord('b') >= ord('a'))) || false end") should
      matchPattern{ case Success(Program(_, _)) => }
    parser.parse("begin int[] a = [1,2,3,4]; int b = (10 / 2 + len a) - ord('b') end") should
      matchPattern{ case Success(Program(_, _)) => }
  }
}
