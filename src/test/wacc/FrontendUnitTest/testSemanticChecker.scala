package frontendUnitTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.ast.ast._
import wacc.ast.Types._
import wacc.errors.WaccError.{ScopeError, TypeError, OtherError}
import wacc.semantic.SemanticCheck.semanticCheck

/* 
  Test the semanticCheck(), which checks for semantic errors
  Covers Left edge cases that were not integrationtest
  Check the error type of semantic error (TypeError, ScopeError, OtherError)

  The test is not related to the parser, as it uses the AST representation as the test input.
*/

class UnitSemanticTest extends AnyFlatSpec with Matchers {
  val p: (Int, Int) = (0, 0)

  "TypeError" should "be detecting wrong types declaration" in {
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), CharLiter('c')(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), BoolLiter(true)(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), StrLiter("hey")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), Bracket(BoolLiter(false)(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
      
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), Bracket(CharLiter('c')(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
      
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), Bracket(StrLiter("hey")(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), NewPair(IntLiter(1)(p), IntLiter(2)(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
  }

  it should "be detecting indirect type errors through assignment" in {
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), IntLiter(1)(p))(p), Assign(Ident("a")(p), CharLiter('c')(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), IntLiter(1)(p))(p), Assign(Ident("a")(p), BoolLiter(true)(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), IntLiter(1)(p))(p), Assign(Ident("a")(p), StrLiter("hi")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), IntLiter(1)(p))(p), Assign(Ident("a")(p), NewPair(IntLiter(1)(p), IntLiter(2)(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
  }

  it should "be able to to parse pair within pair without semantic error" in {
      // begin pair(int, char) r1 = newpair (1, 'a'); pair(char, bool) r2 = newpair ('b', false); pair(pair, pair) r3 = newpair (r1, r2) end
    semanticCheck(Program(List(), List(Declare(PairType(IntType, CharType), Ident("r1")(p), NewPair(IntLiter(1)(p), CharLiter('a')(p))(p))(p), 
                                        Declare(PairType(CharType, BoolType), Ident("r2")(p), NewPair(CharLiter('b')(p), BoolLiter(false)(p))(p))(p), 
                                        Declare(PairType(ErasedPairType, ErasedPairType), Ident("r3")(p), NewPair(Ident("r1")(p), Ident("r2")(p))(p))(p)))(p)) should
      matchPattern{ case Right(_) => }
  }

  it should "prohibit use of non-declared variables in functions" in {
    semanticCheck(Program(List(), List(Free(Ident("s")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : ScopeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), Ident("b")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : ScopeError)) => }

    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), Ident("ab")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : ScopeError)) => }

    semanticCheck(Program(List(), List(Exit(Ident("s")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : ScopeError)) => }
      
    semanticCheck(Program(List(), List(Print(Ident("s")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : ScopeError)) => }
  }

  it should "correctly check names and types of the functions" in {
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), Call(Ident("f")(p), ArgList(List())(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : ScopeError)) => }

    semanticCheck(Program(List(Func(IntType, Ident("sum")(p), List(Param(IntType, Ident("a")(p))(p), Param(IntType, Ident("b")(p))(p)), List(Return(ADD(Ident("a")(p), Ident("b")(p))(p))(p)))(p)), 
                          List(Declare(IntType, Ident("c")(p), Call(Ident("sum")(p), ArgList(List(IntLiter(1)(p), IntLiter(2)(p)))(p))(p))(p)))(p)) should
      matchPattern{ case Right(_) => }

    semanticCheck(Program(List(Func(IntType, Ident("sum")(p), List(Param(IntType, Ident("a")(p))(p), Param(IntType, Ident("b")(p))(p)), List(Return(ADD(Ident("a")(p), Ident("b")(p))(p))(p)))(p)), 
                          List(Declare(CharType, Ident("c")(p), Call(Ident("sum")(p), ArgList(List(IntLiter(1)(p), IntLiter(2)(p)))(p))(100, 0))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
  }

  it should "detect operators used on wrong types" in {
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), ADD(IntLiter(1)(p), CharLiter('c')(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), NEG(BoolLiter(true)(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
  }

  it should "correctly use len" in {
    semanticCheck(Program(List(), List(Declare(StringType, Ident("s")(p), StrLiter("abc")(p))(p), Declare(IntType, Ident("x")(p), LEN(Ident("s")(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
  }

  it should "detect multiple errors" in {
    semanticCheck(Program(List(), List(Free(Ident("s")(p))(p), Free(Ident("s")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : ScopeError, _ : ScopeError)) => }
  }

  it should "detect that char[][] is not be compatible with string[]" in {
    semanticCheck(Program(List(), List(Declare(ArrayType(ArrayType(CharType)), Ident("cpp")(p), ArrayLiter(List(StrLiter("abc")(p), StrLiter("def")(p)))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(ArrayType(ArrayType(CharType)), Ident("cpp")(p), ArrayLiter(List(StrLiter("abc")(p), StrLiter("def")(p)))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }

    semanticCheck(Program(List(), List(Declare(ArrayType(StringType), Ident("sp")(p), ArrayLiter(List(StrLiter("abc")(p), StrLiter("def")(p)))(p))(p), Declare(ArrayType(ArrayType(CharType)), Ident("cpp")(p), Ident("sp")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
  }

  it should "allow string weakening" in {
    semanticCheck(Program(List(), List(Declare(ArrayType(CharType), Ident("cpp")(p), ArrayLiter(List(CharLiter('a')(p), CharLiter('b')(p), CharLiter('c')(p)))(p))(p), Declare(StringType, Ident("s")(p), Ident("cpp")(p))(p)))(p)) should
      matchPattern{ case Right(_) => }
    semanticCheck(Program(List(), List(Declare(ArrayType(CharType), Ident("cpp")(p), ArrayLiter(List(CharLiter('a')(p), CharLiter('b')(p), CharLiter('c')(p)))(p))(p), 
                                       Declare(ArrayType(StringType), Ident("sp")(p), ArrayLiter(List(StrLiter("abc")(p), Ident("cpp")(p)))(p))(p)))(p)) should
      matchPattern{ case Right(_) => }
  }

  it should "string weakening is unidirectional" in {
    semanticCheck(Program(List(), List(Declare(StringType, Ident("s")(p), StrLiter("abc")(p))(p), Declare(ArrayType(CharType), Ident("cpp")(p), Ident("s")(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
    semanticCheck(Program(List(), List(Declare(StringType, Ident("s")(p), StrLiter("abc")(p))(p), Declare(ArrayType(ArrayType(CharType)), Ident("cpp")(p), ArrayLiter(List(Ident("s")(p)))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
  }

  it should "only allow integer exit" in {
    semanticCheck(Program(List(), List(Exit(IntLiter(1)(p))(p)))(p)) should
      matchPattern{ case Right(_) => }
    semanticCheck(Program(List(), List(Exit(BoolLiter(false)(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
    semanticCheck(Program(List(), List(Exit(CharLiter('a')(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
    semanticCheck(Program(List(), List(Exit(OR(BoolLiter(false)(p), BoolLiter(true)(p))(p))(p)))(p)) should 
      matchPattern{ case Left(List(_ : TypeError)) => }
  }

  "Scope" should "prohibit recursive definition"  in {
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), Ident("a")(p))(p)))(p)) should
      matchPattern{ case Left(List(_ : ScopeError)) =>}
  }

  it should "prohibit return in main"  in {
    semanticCheck(Program(List(), List(Return(IntLiter(1)(p))(p)))(p)) should
      matchPattern{ case Left(List(_ : OtherError)) =>}
  }

  it should "prohibit accessing variable outside the function"  in {
    semanticCheck(Program(List(Func(IntType, Ident("f")(p), List(), List(Return(Ident("a")(p))(p)))(p)), List(Declare(IntType, Ident("a")(p), IntLiter(1)(p))(p)))(p)) should
      matchPattern{ case Left(List(_ : ScopeError)) =>}
  }

  it should "allow accessing variable inside if and while"  in {
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), IntLiter(1)(p))(p), IfStmt(BoolLiter(true)(p), List(Print(Ident("a")(p))(p)), List(Print(Ident("a")(p))(p)))(p)))(p)) should
      matchPattern{ case Right(_) => }
    semanticCheck(Program(List(), List(Declare(IntType, Ident("a")(p), IntLiter(1)(p))(p), WhileStmt(BoolLiter(true)(p), List(Print(IntLiter(1)(p))(p)))(p)))(p)) should
      matchPattern{ case Right(_) => }
  }

  it should "prohibit variable access outside if and while scope" in {
    semanticCheck(Program(List(), List(IfStmt(BoolLiter(true)(p), List(Declare(IntType, Ident("a")(p), IntLiter(10)(p))(p)), List(Declare(IntType, Ident("a")(p), IntLiter(5)(p))(p)))(p), Print(Ident("a")(p))(p)))(p)) should
      matchPattern{ case Left(List(_ : ScopeError)) =>}
    semanticCheck(Program(List(), List(WhileStmt(BoolLiter(true)(p), List(Declare(IntType, Ident("a")(p), IntLiter(10)(p))(p)))(p), Print(Ident("a")(p))(p)))(p)) should
      matchPattern{ case Left(List(_ : ScopeError)) =>}
  }
  
  "Testing" should "parse this" in {
      // parser.parseFile(new File("test_cases/valid/function/nested_functions/fibonacciFullRec.wacc")) shouldBe
      //     null
      // parser.parse("begin if true then int a = 10 else int a = 5 fi; print a end") shouldBe
      //     None
  }
}
