package frontendUnitTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.Success
import wacc.ast.ast._
import wacc.ast.Types._
import wacc.syntax.parser

/*
  Test parser
  Parse a valid wacc code 
  and check if it generates a correct AST of that code
*/

class UnitTest extends AnyFlatSpec with Matchers {
  val p: (Int, Int) = (0, 0)

  "Skip" should "be able to parse simple statements" in {
    parser.parse("begin skip end") should 
      matchPattern { 
        case Success(Program(List(), List(Skip()))) =>
      }
    parser.parse("begin exit -1 end") should 
      matchPattern { 
        case Success(Program(List(), List(Exit(IntLiter(-1))))) =>
      }
    parser.parse("begin print \"hello world\" end") should 
      matchPattern { 
        case Success(Program(List(), List(Print(StrLiter("hello world"))))) =>
      }
    parser.parse("begin println \"hello world\" end") should 
      matchPattern { 
        case Success(Program(List(), List(Println(StrLiter("hello world"))))) =>
      }
    parser.parse("begin int a = 0; read a end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(IntType, Ident("a"), IntLiter(0)), Read(Ident("a"))))) =>
      }
  }

  "Declare" should "be able to parse boolean variable declaration" in {
    parser.parse("begin bool b = false end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(BoolType, Ident("b"), BoolLiter(false))))) =>
      }
    parser.parse("begin bool b = true end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(BoolType, Ident("b"), BoolLiter(true))))) =>
      }
  }

  it should "be able to parse int declaration with arbitrary length" in {
    parser.parse("begin int i = 1 end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(1))))) =>
      }
    parser.parse("begin int i = 12 end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(12))))) =>
      }
    parser.parse("begin int i = 1234567 end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(1234567))))) =>
      }
    parser.parse("begin int i = 2147483647 end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(2147483647))))) =>
      }

    parser.parse("begin int i = 0 end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(0))))) =>
      }
  }

    it should "be able to parse negative int declaration with arbitrary length" in {
      parser.parse("begin int i = -1 end") should 
        matchPattern {
          case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(-1))))) =>
        }
      parser.parse("begin int i = -12 end") should 
        matchPattern {
          case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(-12))))) =>
        }
      parser.parse("begin int i = -123456789 end") should 
        matchPattern {
          case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(-123456789))))) =>
        }
      parser.parse("begin int i = -2147483648 end") should 
        matchPattern {
          case Success(Program(List(), List(Declare(IntType, Ident("i"), IntLiter(-2147483648))))) =>
        }
  }

  it should "be able to parse character declaration" in {
    parser.parse("begin char c = 'c' end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(CharType, Ident("c"), CharLiter('c'))))) =>
      }
    parser.parse("begin char c = '0' end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(CharType, Ident("c"), CharLiter('0'))))) =>
      }
    parser.parse("begin char c = '\\n' end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(CharType, Ident("c"), CharLiter('\n'))))) =>
      }
    parser.parse("begin char c = ' ' end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(CharType, Ident("c"), CharLiter(' '))))) =>
      }
  }

  it should "be able to parse string declaration with arbitrary length" in {
    parser.parse("begin string str = \"\" end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(StringType, Ident("str"), StrLiter(""))))) =>
      }
    parser.parse("begin string str = \"a\" end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(StringType, Ident("str"), StrLiter("a"))))) =>
      }
    parser.parse("begin string str = \"Hello World! 1234\\n\" end") should 
      matchPattern {
          case Success(Program(List(), List(Declare(StringType, Ident("str"), StrLiter("Hello World! 1234\n"))))) =>
      }
  }


  it should "be able to parse pair declaration" in {
    parser.parse("begin pair(int, char) pr = newpair (1, 'c') end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(PairType(IntType, CharType), Ident("pr"), NewPair(IntLiter(1), CharLiter('c')))))) =>
      }
  }

  it should "be able to parse pair in pair declaration" in {
    parser.parse("begin pair(int, char) p1 = newpair (1, 'c'); pair (string, pair) p2 = newpair(\"hey\", p1) end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(PairType(IntType, CharType), Ident("p1"), NewPair(IntLiter(1), CharLiter('c'))), Declare(PairType(StringType, ErasedPairType), Ident("p2"), NewPair(StrLiter("hey"), Ident("p1"))))))  =>
      }
    parser.parse("begin pair(int, char) p1 = newpair (1, 'c'); pair (pair, string) p2 = newpair(p1, \"hey\") end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(PairType(IntType, CharType), Ident("p1"), NewPair(IntLiter(1), CharLiter('c'))), Declare(PairType(ErasedPairType, StringType), Ident("p2"), NewPair(Ident("p1"), StrLiter("hey")))))) =>
      }
                  
  }

  it should "be able to parse array declaration" in {
    parser.parse("begin int[] arr = [1, 2, 3] end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(ArrayType(IntType), Ident("arr"), ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3))))))) =>
      }
  }
  
  it should "be able to understand array literal" in {
    parser.parse("begin int[] arr = [1, 2, 3]; arr[1] = arr[2] + 1 end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(ArrayType(IntType), Ident("arr"), ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3)))), Assign(ArrayElem(Ident("arr"), List(IntLiter(1))), ADD(ArrayElem(Ident("arr"), List(IntLiter(2))), IntLiter(1)))))) =>
      }
    parser.parse("begin int[] arr = [1, 2, 3]; print arr[1] end") should
      matchPattern {
        case Success(Program(List(), List(Declare(ArrayType(IntType), Ident("arr"), ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3)))), Print(ArrayElem(Ident("arr"), List(IntLiter(1))))))) =>
      }
  }

  it should "be able to parse array in array declaration" in {
    parser.parse("begin int[] arr = [1, 2, 3]; int[][] arr2 = [arr, arr] end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(ArrayType(IntType), Ident("arr"), ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3)))), Declare(ArrayType(ArrayType(IntType)), Ident("arr2"), ArrayLiter(List(Ident("arr"), Ident("arr"))))))) =>
      }
    parser.parse("begin int[] arr = [1, 2, 3]; int[][] arr2 = [arr, arr]; int[][][] arr3 = [arr2, arr2] end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(ArrayType(IntType), Ident("arr"), ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3)))), Declare(ArrayType(ArrayType(IntType)), Ident("arr2"), ArrayLiter(List(Ident("arr"), Ident("arr")))), Declare(ArrayType(ArrayType(ArrayType(IntType))), Ident("arr3"), ArrayLiter(List(Ident("arr2"), Ident("arr2"))))))) =>
      }
  }

  it should "be able to parse pair in array declaration" in {
    parser.parse("begin int[] arr = [1, 2, 3] end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(ArrayType(IntType), Ident("arr"), ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3))))))) =>
      }
  }

  "Expressions" should "be able to parse boolean expressions" in {
    parser.parse("begin bool b = true || false end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(BoolType, Ident("b"), OR(BoolLiter(true), BoolLiter(false)))))) =>
      }
    parser.parse("begin bool b = false && true end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(BoolType, Ident("b"), AND(BoolLiter(false), BoolLiter(true)))))) =>
      }
    parser.parse("begin bool b = false && true || true end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(BoolType, Ident("b"), OR(AND(BoolLiter(false), BoolLiter(true)), BoolLiter(true)))))) =>
      }
    parser.parse("begin bool b = false || true && true end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(BoolType, Ident("b"), OR(BoolLiter(false), AND(BoolLiter(true), BoolLiter(true))))))) =>
      }
    parser.parse("begin bool b = ! true || ! ! false end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(BoolType, Ident("b"), OR(NOT(BoolLiter(true)), NOT(NOT(BoolLiter(false)))))))) =>
      }
    parser.parse("begin bool b = (! !false || !true) && true end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(BoolType, Ident("b"), AND(Bracket(OR(NOT(NOT(BoolLiter(false))), NOT(BoolLiter(true)))), BoolLiter(true)))))) =>
      }
  }

  it should "be able to parse integer expressions" in {
    parser.parse("begin int x = 1 * 3 + 2 - 3 * 4 / 2 end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(IntType, Ident("x"), SUB(ADD(MUL(IntLiter(1), IntLiter(3)), IntLiter(2)), DIV(MUL(IntLiter(3), IntLiter(4)), IntLiter(2))))))) =>
      }
  }

  it should "be able to parse unary operators" in {
    parser.parse("begin int[] arr = [1, 2, 3]; int length = len arr end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(ArrayType(IntType), Ident("arr"), ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3)))), Declare(IntType, Ident("length"), LEN(Ident("arr")))))) =>
      }

    parser.parse("begin int a = ord 'c' end") should 
      matchPattern {
        case Success(Program(List(), List(Declare(IntType, Ident("a"), ORD(CharLiter('c')))))) =>
      }
  }

  "Functions" should "be able to parse functions" in {
    parser.parse("begin int[] f(int a) is int[] x = [1,2,3]; return x end int[] x = call f(10) end") should 
      matchPattern {
        case  Success(Program(List(Func(ArrayType(IntType), Ident("f"), List(Param(IntType, Ident("a"))), List(Declare(ArrayType(IntType), Ident("x"), ArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3)))), Return(Ident("x"))))), List(Declare(ArrayType(IntType), Ident("x"), Call(Ident("f"), ArgList(List(IntLiter(10)))))))) =>
      }

    parser.parse("begin bool f(int x, int y, int z) is return (x + y + z) > 10 end int[] x = call f(1, 2, 3) end") should 
      matchPattern {
        case  Success(Program(List(Func(BoolType, Ident("f"), List(Param(IntType, Ident("x")), Param(IntType, Ident("y")), Param(IntType, Ident("z"))), List(Return(GT(Bracket(ADD(ADD(Ident("x"), Ident("y")), Ident("z"))), IntLiter(10)))))), List(Declare(ArrayType(IntType), Ident("x"), Call(Ident("f"), ArgList(List(IntLiter(1), IntLiter(2), IntLiter(3)))))))) =>
      }

  }

  "Functions" should "always exit" in {
    parser.parse("begin int f() is int a = 1 end print 1 end") should
      matchPattern {
        case parsley.Failure(_) =>
      }
    
    parser.parse("begin int f() is if true then return 0 else print 10 end print 1 end") should
      matchPattern {
        case parsley.Failure(_) =>
      }
    
    parser.parse("begin int f() is while true do return 0 done print 10 end print 1 end") should
      matchPattern {
        case parsley.Failure(_) =>
      }

    parser.parse("begin int f() is \n while \n true \n do \n print 0 \n done; \n return 10 end print 1 end") should
      matchPattern {
        case  Success(Program(List(Func(IntType, Ident("f"), List(), List(WhileStmt(BoolLiter(true), List(Print(IntLiter(0)))), Return(IntLiter(10))))), List(Print(IntLiter(1))))) =>
      }
  }

  "Test" should "parse this" in {
    // parser.parse("begin int sum(int x, int y) is return x + y end int x = call sum(3, 4); println(x) end") shouldBe 0
  }
}