package wacc.ast

import parsley.Parsley._
import parsley.generic._

import ParserBridgePos._
import Types._

object ast {

  // Base trait for nodes that include position information (line and column numbers).
  sealed trait Pos { val pos: (Int, Int) }

  // Atoms represent the simplest elements in expressions.
  sealed trait Atom                                         extends Expr
  case class IntLiter(value: Int)(val pos: (Int, Int))      extends Atom
  case class BoolLiter(value: Boolean)(val pos: (Int, Int)) extends Atom
  case class CharLiter(value: Char)(val pos: (Int, Int))    extends Atom
  case class StrLiter(value: String)(val pos: (Int, Int))   extends Atom
  case class PairLiter()(val pos: (Int, Int))               extends Atom
  case class Ident(name: String)(val pos: (Int, Int))      extends Atom with lValue
  case class Bracket(value: Expr)(val pos: (Int, Int))      extends Atom

  // Special atom for array element access.
  sealed trait IdentForArray                                                extends Atom with lValue
  case class ArrayElem(ident: Ident, expr: List[Expr])(val pos: (Int, Int)) extends IdentForArray

  // Parser bridge objects to facilitate the creation of AST nodes with position information.
  object IntLiter  extends ParserBridgePos1[Int, IntLiter]
  object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]
  object CharLiter extends ParserBridgePos1[Char, CharLiter]
  object StrLiter  extends ParserBridgePos1[String, StrLiter]
  object PairLiter extends ParserBridgePos0[PairLiter]

  object Ident     extends ParserBridgePos1[String, Ident]
  object ArrayElem extends ParserBridgePos2[Ident, List[Expr], IdentForArray]
  object Bracket   extends ParserBridgePos1[Expr, Expr]

  /*
    Statements representing executable constructs and control flow within the program.
    Definitions for functions, parameters, and various statement types with position information.
  */  

  // Program
  case class Program(funcs: List[Func], stmts: List[Stmt])(val pos: (Int, Int))
  object Program extends ParserBridgePos2[List[Func], List[Stmt], Program]

  // Function
  case class Func(funcType: Type, ident: Ident, param_list: List[Param], stmts: List[Stmt])(
    val pos: (Int, Int)
  )
  object Func extends ParserBridgePos4[Type, Ident, List[Param], List[Stmt], Func]

  // Parameter
  case class Param(paramType: Type, ident: Ident)(val pos: (Int, Int))
  object Param extends ParserBridgePos2[Type, Ident, Param]

  // Statement
  sealed trait Stmt
  case class Skip()(val pos: (Int, Int)) extends Stmt
  case class Declare(identType: Type, ident: Ident, rvalue: rValue)(
    val pos: (Int, Int)
  ) extends Stmt
  case class Assign(lvalue: lValue, rvalue: rValue)(val pos: (Int, Int)) extends Stmt
  case class Read(lvalue: lValue)(val pos: (Int, Int))                   extends Stmt
  case class Free(expr: Expr)(val pos: (Int, Int))                       extends Stmt
  case class Return(expr: Expr)(val pos: (Int, Int))                     extends Stmt
  case class Exit(expr: Expr)(val pos: (Int, Int))                       extends Stmt
  case class Print(expr: Expr)(val pos: (Int, Int))                      extends Stmt
  case class Println(expr: Expr)(val pos: (Int, Int))                    extends Stmt
  case class IfStmt(expr: Expr, thenStmt: List[Stmt], 
                    elseStmt: List[Stmt])(val pos: (Int, Int))           extends Stmt
  case class WhileStmt(expr: Expr, stmts: List[Stmt])
                      (val pos: (Int, Int))                              extends Stmt
  case class Scope(stmt: List[Stmt])(val pos: (Int, Int))                extends Stmt

  object Skip extends ParserBridgePos0[Skip] {
    def apply()(pos: (Int, Int)): Skip = new Skip()(pos)
  }
  
  // ParserBridge connected to Statements
  object Declare   extends ParserBridgePos3[Type, Ident, rValue, Declare]
  object Assign    extends ParserBridgePos2[lValue, rValue, Assign]
  object Read      extends ParserBridgePos1[lValue, Read]
  object Free      extends ParserBridgePos1[Expr, Free]
  object Return    extends ParserBridgePos1[Expr, Return]
  object Exit      extends ParserBridgePos1[Expr, Exit]
  object Print     extends ParserBridgePos1[Expr, Print]
  object Println   extends ParserBridgePos1[Expr, Println]
  object IfStmt    extends ParserBridgePos3[Expr, List[Stmt], List[Stmt], IfStmt]
  object WhileStmt extends ParserBridgePos2[Expr, List[Stmt], WhileStmt]
  object Scope     extends ParserBridgePos1[List[Stmt], Scope]

  // LValue and RValue
  sealed trait lValue extends Pos
  sealed trait PairElem extends lValue with rValue
  sealed trait rValue extends Pos
  case class ArrayLiter(value: List[Expr])(val pos: (Int, Int))             extends rValue
  case class NewPair(val expr1: Expr, val expr2: Expr)(val pos: (Int, Int)) extends rValue
  object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]
  case class Call(val ident: Ident, val args: ArgList)(val pos: (Int, Int)) extends rValue
  object Call extends ParserBridgePos2[Ident, ArgList, rValue]

  // Argument List
  case class ArgList(value: List[Expr])(val pos: (Int, Int))
  object ArgList extends ParserBridgePos1[List[Expr], ArgList]

  // Fst and Snd of Pair Element
  case class Fst(value: lValue)(val pos: (Int, Int)) extends PairElem
  object Fst                                         extends ParserBridgePos1[lValue, Fst]
  case class Snd(value: lValue)(val pos: (Int, Int)) extends PairElem
  object Snd                                         extends ParserBridgePos1[lValue, Snd]

  // Array Literal
  object ArrayLiter extends ParserBridgePos1[List[Expr], ArrayLiter]

  // Expressions: Constructs that evaluate to values, including operations and function calls.
  sealed trait Expr extends rValue
  sealed trait UnaryOp  extends Expr { val expr: Expr                   }
  sealed trait BinaryOp extends Expr { val lExpr: Expr; val rExpr: Expr }
  
  // Unary Operations
  abstract class UnOpParserBridge extends ParserBridgePos1[Expr, Expr] {
    override def labels: List[String] = List("Unary Operator")
    override def reason: Option[String] = Some("Accepted unary operators are: NOT NEG LEN ORD CHR")
  }

  case class NOT(expr: Expr)(val pos: (Int, Int)) extends UnaryOp
  case class NEG(expr: Expr)(val pos: (Int, Int)) extends UnaryOp
  case class LEN(expr: Expr)(val pos: (Int, Int)) extends UnaryOp
  case class ORD(expr: Expr)(val pos: (Int, Int)) extends UnaryOp
  case class CHR(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  // ParserBridge connected to Unary Operations Object
  object NOT extends UnOpParserBridge
  object NEG extends UnOpParserBridge
  object LEN extends UnOpParserBridge
  object ORD extends UnOpParserBridge
  object CHR extends UnOpParserBridge

  // Binary Operations
  abstract class BinOpParserBridge extends ParserBridgePos2[Expr, Expr, Expr] {
    override def labels: List[String] = List("Binary Operator")
  }

  case class MUL(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class DIV(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class MOD(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class ADD(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class SUB(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class GT(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int))  extends BinaryOp
  case class GTE(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class LT(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int))  extends BinaryOp
  case class LTE(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class EQ(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int))  extends BinaryOp
  case class NEQ(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class AND(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int)) extends BinaryOp
  case class OR(lExpr: Expr, rExpr: Expr)(val pos: (Int, Int))  extends BinaryOp

  // ParserBridge connected to Binoary Operations Object
  object MUL extends BinOpParserBridge
  object DIV extends BinOpParserBridge
  object MOD extends BinOpParserBridge
  object ADD extends BinOpParserBridge
  object SUB extends BinOpParserBridge
  object GT  extends BinOpParserBridge
  object GTE extends BinOpParserBridge
  object LT  extends BinOpParserBridge
  object LTE extends BinOpParserBridge
  object EQ  extends BinOpParserBridge
  object NEQ extends BinOpParserBridge
  object AND extends BinOpParserBridge
  object OR  extends BinOpParserBridge
}
