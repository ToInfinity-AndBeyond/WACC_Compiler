package backendUnitTest

import wacc.ast.ast._
import wacc.ast.Types._

object TestCases {

  val p: (Int, Int) = (0, 0)

  val andExpr: Program = Program(
    List(),
    List(
      Declare(BoolType,Ident("a")(p),BoolLiter(true)(p))(p), 
      Declare(BoolType,Ident("b")(p),BoolLiter(false)(p))(p), 
      Println(AND(Ident("a")(p),Ident("b")(p))(p))(p), 
      Println(AND(Ident("a")(p),BoolLiter(true)(p))(p))(p), 
      Println(AND(Ident("b")(p),BoolLiter(false)(p))(p))(p)
    )
  )(p)
  val equalsExpr: Program = Program(
    List(),
    List(
      Declare(IntType, Ident("x")(p), IntLiter(2)(p))(p),
      Declare(IntType, Ident("y")(p), IntLiter(4)(p))(p),
      Declare(IntType, Ident("z")(p), IntLiter(4)(p))(p),
      Declare(BoolType, Ident("b")(p), EQ(Ident("x")(p), Ident("y")(p))(p))(p),
      Println(Ident("b")(p))(p),
      Println(EQ(Ident("x")(p), Ident("y")(p))(p))(p),
      Println(EQ(Ident("y")(p), Ident("z")(p))(p))(p)
    )
  )(p)
  val greaterExpr: Program = Program(
    List(),
    List(
      Declare(IntType,Ident("x")(p),IntLiter(2)(p))(p),
      Declare(IntType,Ident("y")(p),IntLiter(6)(p))(p),
      Declare(IntType,Ident("z")(p),IntLiter(4)(p))(p),
      Println(GT(Ident("x")(p),Ident("y")(p))(p))(p),
      Println(GT(Ident("y")(p),Ident("z")(p))(p))(p)
    )
  )(p)
  val plusExpr: Program = Program(
    List(),
    List(
      Declare(IntType, Ident("x")(p), IntLiter(15)(p))(p),
      Declare(IntType, Ident("y")(p), IntLiter(20)(p))(p),
      Println(ADD(Ident("x")(p), Ident("y")(p))(p))(p)
    )
  )(p)
  val minusExpr: Program = Program(
    List(),
    List(
      Declare(IntType, Ident("x")(p), IntLiter(15)(p))(p),
      Declare(IntType, Ident("y")(p), IntLiter(20)(p))(p),
      Println(SUB(Ident("y")(p), Ident("x")(p))(p))(p)
    )
  )(p)
  val multExpr: Program = Program(
    List(),
    List(
      Declare(IntType, Ident("x")(p), IntLiter(5)(p))(p),
      Declare(IntType, Ident("y")(p), IntLiter(3)(p))(p),
      Println(MUL(Ident("x")(p), Ident("y")(p))(p))(p)
    )
  )(p)
  val negExpr: Program = Program(
    List(),
    List(
      Declare(IntType, Ident("x")(p), IntLiter(42)(p))(p),
      Println(NEG(Ident("x")(p))(p))(p)
    )
  )(p)

  val negExpr2: Program = Program(
    List(),
    List(
      Declare(IntType, Ident("x")(p), IntLiter(42)(p))(p),
      Declare(IntType, Ident("y")(p), NEG(IntLiter(42)(p))(p))(p),
      Println(NEG(NEG(NEG((Ident("y")(p)))(p))(p))(p))(p)
    )
  )(p)

  val orExpr: Program = Program(
    List(),
    List(
      Declare(BoolType, Ident("a")(p), BoolLiter(true)(p))(p),
      Declare(BoolType, Ident("b")(p), BoolLiter(false)(p))(p),
      Println(OR(Ident("a")(p), Ident("b")(p))(p))(p),
      Println(OR(Ident("a")(p), BoolLiter(true)(p))(p))(p),
      Println(OR(Ident("b")(p), BoolLiter(false)(p))(p))(p)
    )
  )(p)
  val notequalsExpr: Program = Program(
    List(),
    List(
      Declare(IntType, Ident("x")(p), IntLiter(2)(p))(p),
      Declare(IntType, Ident("y")(p), IntLiter(4)(p))(p),
      Declare(IntType, Ident("z")(p), IntLiter(4)(p))(p),
      Declare(BoolType, Ident("b")(p), NEQ(Ident("x")(p), Ident("y")(p))(p))(p),
      Println(Ident("b")(p))(p),
      Println(NEQ(Ident("x")(p), Ident("y")(p))(p))(p),
      Println(NEQ(Ident("y")(p), Ident("z")(p))(p))(p)
    )
  )(p)

  val stringEqualsExpr: Program = Program(
    List(),
    List(
      Declare(StringType, Ident("s1")(p), StrLiter("Hello")(p))(p),
      Declare(StringType, Ident("s2")(p), StrLiter("foo")(p))(p),
      Declare(StringType, Ident("s3")(p), StrLiter("bar")(p))(p),
      Declare(BoolType, Ident("b")(p), EQ(Ident("s1")(p), Ident("s1")(p))(p))(p),
      Println(Ident("b")(p))(p),
      Println(EQ(Ident("s1")(p), Ident("s2")(p))(p))(p),
      Println(EQ(Ident("s2")(p), Ident("s3")(p))(p))(p)
    )
  )(p)

  val stringEqualsExpr2: Program = Program(
    List(),
    List(
      Declare(StringType, Ident("s1")(p), StrLiter("Hello")(p))(p),
      Declare(StringType, Ident("s2")(p), StrLiter("foo")(p))(p),
      Declare(StringType, Ident("s3")(p), StrLiter("Hello")(p))(p),
      Declare(BoolType, Ident("b")(p), EQ(Ident("s1")(p), Ident("s2")(p))(p))(p),
      Println(Ident("b")(p))(p),
      Println(EQ(Ident("s1")(p), Ident("s3")(p))(p))(p),
      Println(EQ(Ident("s3")(p), Ident("s2")(p))(p))(p)
    )
  )(p)

  val assignExpr: Program = Program(
    List(),
    List(
      Declare(IntType, Ident("x")(p), IntLiter(2)(p))(p),
      Declare(IntType, Ident("y")(p), IntLiter(3)(p))(p),
      Assign(Ident("y")(p), Ident("x")(p))(p),
      Assign(Ident("x")(p), IntLiter(4)(p))(p),
      Println(Ident("x")(p))(p),
      Println(Ident("y")(p))(p),
    )
  )(p)

  val functionExpr: Program = 
    Program(List(Func(IntType, Ident("sum")(p), List(Param(IntType, Ident("x")(p))(p), Param(IntType, Ident("y")(p))(p)),
               List(Return(ADD(Ident("x")(p), Ident("y")(p))(p))(p)))(p)), 
          List(
            Declare(IntType, Ident("x")(p), Call(Ident("sum")(p), ArgList(List(IntLiter(3)(p), IntLiter(4)(p)))(p))(p))(p), 
            Println(Ident("x")(p))(p)))(p)

  val functionRecExpr: Program =
    Program(List(Func(IntType, Ident("fibonacci")(p), List(Param(IntType, Ident("n")(p))(p)), List(
      IfStmt(LTE(Ident("n")(p), IntLiter(1)(p))(p), List(Return(Ident("n")(p))(p)), List(Skip()(p)))(p), 
      Declare(IntType, Ident("f1")(p), Call(Ident("fibonacci")(p), 
      ArgList(List(SUB(Ident("n")(p), IntLiter(1)(p))(p)))(p))(p))(p), 
      Declare(IntType, Ident("f2")(p), 
      Call(Ident("fibonacci")(p), 
      ArgList(List(SUB(Ident("n")(p), IntLiter(2)(p))(p)))(p))(p))(p), 
      Return(ADD(Ident("f1")(p), Ident("f2")(p))(p))(p)))(p)), 
      List( 
      Declare(IntType, Ident("n")(p), IntLiter(0)(p))(p), Read(Ident("n")(p))(p), 
      Println(Ident("n")(p))(p),
      Declare(IntType, Ident("result")(p), Call(Ident("fibonacci")(p), 
      ArgList(List(Ident("n")(p)))(p))(p))(p), Println(Ident("result")(p))(p)))(p)

}

