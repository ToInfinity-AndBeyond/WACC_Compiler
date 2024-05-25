package wacc.optimization

import wacc.ast.Types._
import wacc.ast.ast._
import wacc.errors.WaccError._
import wacc.semantic.SymbolTree
import wacc.optimization.ConstantState

import scala.collection.mutable
import wacc.ir.expressionTranslator
import wacc.syntax.lexer

object constantFolding {
  private final val p = (0, 0)
  private final val CHR_MAX = 127
  private final val CHR_MIN = 0

  /*  Constant Folding
      Evaluate an expression to constant if possible.
      If not possible, return None */ 
  def expressionToConstant(expr: Expr, state: ConstantState): Option[Int] = {
    expr match {
      case Ident(name) => {
        state.getIdentType(name) match {
          case IntType | BoolType | CharType => state.getIdentValue(name)
          case _ => None
        }
      }
      case IntLiter(v) => Some(v)
      case BoolLiter(v) => Some(if (v) 1 else 0)
      case CharLiter(v) => Some(v.toInt)
      case binOp: BinaryOp => binOpToConstant(binOp, state)
      case unOp: UnaryOp => unOpToConstant(unOp, state)
      case Bracket(e) => expressionToConstant(e, state)
      case _ => None
    }
  }

  /* Evaluate binary operator to constant, if not possible, return None */
  private def binOpToConstant(binOp: BinaryOp, state: ConstantState): Option[Int] = {
    val position = binOp.pos

    binOp match {
      case MUL(e1, e2) => combineBinOpToConstant(e1, e2, state, _ * _, "multiply", position)
      case DIV(e1, e2) => {
        expressionToConstant(e2, state) match {
          case None => None
          case Some(value) => {
            if (value == 0) 
              throw new RuntimeException(s"Divide by zero: trying to divide by 0 $position")
          }
        }
        combineBinOpToConstant(e1, e2, state, _ / _, "divide", position)
      }
      case MOD(e1, e2) => {
        expressionToConstant(e2, state) match {
          case None => None
          case Some(value) => {
            if (value == 0) 
              throw new RuntimeException(s"Mod by zero: trying to mod by 0 $position")
          }
        }
        combineBinOpToConstant(e1, e2, state, _ % _, "mod", position)
      }
      case ADD(e1, e2) => combineBinOpToConstant(e1, e2, state, _ + _, "add", position)
      case SUB(e1, e2) => combineBinOpToConstant(e1, e2, state, _ - _, "subtract", position)
      case GT(e1, e2) => combineBinOpToConstant(e1, e2, state, (a, b) => if(a > b) 1 else 0)
      case GTE(e1, e2) => combineBinOpToConstant(e1, e2, state, (a, b) => if(a >= b) 1 else 0)
      case LT(e1, e2) => combineBinOpToConstant(e1, e2, state, (a, b) => if(a < b) 1 else 0)
      case LTE(e1, e2) => combineBinOpToConstant(e1, e2, state, (a, b) => if(a <= b) 1 else 0)
      case EQ(e1, e2) => combineBinOpToConstant(e1, e2, state, (a, b) => if(a == b) 1 else 0)
      case NEQ(e1, e2) => combineBinOpToConstant(e1, e2, state, (a, b) => if(a != b) 1 else 0)
      case AND(e1, e2) => combineBinOpToConstant(e1, e2, state, (a, b) => if((a + b) == 2) 1 else 0)
      case OR(e1, e2) => combineBinOpToConstant(e1, e2, state, (a, b) => if((a + b) != 0) 1 else 0)
    }
  }

  private def combineBinOpToConstant(e1: Expr, e2: Expr, state: ConstantState, operation: (Long, Long) => Long, opName: String = "none", position: (Int, Int) = p): Option[Int] = {
    val v1 = expressionToConstant(e1, state)
    val v2 = expressionToConstant(e2, state)

    (v1, v2) match {
      case (Some(c1), Some(c2)) => {
        val res = operation(c1.toLong, c2.toLong)

        if (res > Int.MaxValue)
          throw new RuntimeException(s"Integer Overflow: $opName constant $c1 to constant $c2 $position")
        if (res < Int.MinValue)
          throw new RuntimeException(s"Integer Underflow: $opName constant $c1 to constant $c2 $position")

        Some(res.toInt)
        // Some(operation(c1, c2))
      }
      case _ => None
    }
  }

  /* Evaluate unary operator to constant value, if not possible, return None */
  private def unOpToConstant(unOp: UnaryOp, state: ConstantState): Option[Int] = {
    unOp match {
      case CHR(e) => {
        expressionToConstant(e, state) match {
          case None => None
          case Some(value) => {
            val position = unOp.pos
            if (value > CHR_MAX) 
              throw new RuntimeException(s"CHR Overflow: trying to use chr to $value $position")
            if (value < CHR_MIN)
              throw new RuntimeException(s"CHR Underflow: trying to use chr to $value $position")
            Some(value)
          }
        }
      }
      case ORD(e) => expressionToConstant(e, state)
      case LEN(Ident(name)) => None
      case LEN(_) => None
      case NEG(e) => {
        expressionToConstant(e, state) match {
          case None => None
          case Some(v) => {
            val position = unOp.pos
            if (-v.toLong > Int.MaxValue) 
              throw new RuntimeException(s"Integer Overflow: trying to use negation to $v $position")
            if (-v.toLong < Int.MinValue)
              throw new RuntimeException(s"Integer Underflow: trying to use negation to $v $position")
            Some(-v)
          }
        }
      }
      case NOT(e) => {
        expressionToConstant(e, state) match {
          case None => None
          case Some(v) => Some(if (v == 0) 1 else 0)
        }
      }
    }
  }
}