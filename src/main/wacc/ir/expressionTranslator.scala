package wacc.ir

import wacc.ast.ast._
import wacc.ast.Types
import Consts._
import threeAddrCode._ 
import scala.collection.immutable
import wacc.ast.Types.ArrayType

object Consts {
  val TRUE = 1
  val FALSE = 0
  val NULL = 0
}

object expressionTranslator {
  // convert an expression to a list of three address instructions and the location of the result
  def translateExpression(expr: Expr, state: State): (TLoc, List[ThreeAddrInstr]) = {
    val res: (TLoc, List[ThreeAddrInstr]) = expr match {
      //Atoms
      case IntLiter(value)  => {
        val loc = state.curLocTable.getNextTRegLoc()
        (loc, List(TAssignImm(loc, new TImm(value))))
      }
      case BoolLiter(bool) => {
        val value = if (bool) TRUE else FALSE
        val loc = state.curLocTable.getNextTRegLoc()
        (loc, List(TAssignImm(loc, new TImm(value))))
      }
      case CharLiter(value) => {
        val loc = state.curLocTable.getNextTRegLoc()
        (loc, List(TAssignImm(loc, new TImm(value.toInt))))
      }
        
      case StrLiter(value)  => {
        val strLabel = state.stringTable.addString(value)
        val loc = state.curLocTable.getNextTRegLoc()
        (loc, List(TStrLoad(loc, strLabel)))
      }
      case Ident(value) => {
        val varLoc = state.curLocTable.lookUpIdent(value)
        val loc = state.curLocTable.getNextTRegLoc()
        (loc, List(TAssignLoc(loc, varLoc)))
      }
      case Bracket(value) => translateExpression(value, state)

      case ArrayElem(ident, expr) => translateArrayElem(ident, expr, state)
      case PairLiter() => {
        val loc = state.curLocTable.getNextTRegLoc()
        (loc, List(TAssignImm(loc, new TImm(NULL))))
      }
      case unOp: UnaryOp => translateUnOp(unOp, state)
      case binOp: BinaryOp => translateBinOp(binOp, state)
    }
    res
  }

  // def tryTranslateExpressionToConstant(expr: Expr, state: State): Either[(TLoc, List[ThreeAddrInstr]), TImm] = {
  //   evaluateExpressionToConstant(expr, state) match {
  //     case None => Left(translateExpression(expr, state))
  //     case Some(constantVal) => Right(constantVal)
  //   }
  // }

  // Get expression as a constant and assign to a new RegLoc if possible
  // If not possible, just translateExpression
  def getExpressionAsConstantLoc(expr: Expr, state: State): (TLoc, List[ThreeAddrInstr]) = {
    state.enterLocScope()
    val constantVal = evaluateExpressionToConstant(expr, state)

    val res = constantVal match {
      case None => translateExpression(expr, state)
      case Some(value) => {
        val resLoc = state.curLocTable.getNextTRegLoc()
        (resLoc, List(TAssignImm(resLoc, value)))
      }
    }
    state.exitLocScope()

    res
  }

  def evaluateExpressionToConstant(expr: Expr, state: State): Option[TImm] = {
    expr match {
      case IntLiter(value) => Some(new TImm(value))
      case BoolLiter(bool) => Some(new TImm(if (bool) TRUE else FALSE))
      case CharLiter(value) => Some(new TImm(value.toInt))
      case Bracket(value) => evaluateExpressionToConstant(value, state)
      case Ident(name) => {
        state.curLocTable.getIdentConstant(name) match {
          case Some(value) => Some(value)
          case None => None
        }
      }
      case ArrayElem(_, _) => None
      case StrLiter(_) => None
      case PairLiter() => None
      case binOp: BinaryOp => evaluateBinOpToConstant(binOp, state)
      case unOp: UnaryOp => evaluateUnOpToConstant(unOp, state)
    }
  }

  // Helper function for evaluateBinOpToConstants
  private def evaluateBinOpCombine(e1: Expr, e2: Expr, state: State, operation: (Int, Int) => Int): Option[TImm] = {
    val v1 = evaluateExpressionToConstant(e1, state)
    val v2 = evaluateExpressionToConstant(e2, state)
    (v1, v2) match {
      case (Some(c1), Some(c2)) => Some(new TImm(operation(c1.value, c2.value)))
      case _ => None
    }
  }

  // Evaluate a BinaryOp into a Constant Imm value
  // If not possible, return None
  private def evaluateBinOpToConstant(binOp: BinaryOp, state: State): Option[TImm] = {
    binOp match {
      case MUL(e1, e2) => evaluateBinOpCombine(e1, e2, state, _ * _)
      case DIV(e1, e2) => evaluateBinOpCombine(e1, e2, state, _ / _)
      case MOD(e1, e2) => evaluateBinOpCombine(e1, e2, state, _ % _)
      case ADD(e1, e2) => evaluateBinOpCombine(e1, e2, state, _ + _)
      case SUB(e1, e2) => evaluateBinOpCombine(e1, e2, state, _ - _)
      case GT(e1, e2) => evaluateBinOpCombine(e1, e2, state, (a, b) => if(a > b) 1 else 0)
      case GTE(e1, e2) => evaluateBinOpCombine(e1, e2, state, (a, b) => if(a >= b) 1 else 0)
      case LT(e1, e2) => evaluateBinOpCombine(e1, e2, state, (a, b) => if(a < b) 1 else 0)
      case LTE(e1, e2) => evaluateBinOpCombine(e1, e2, state, (a, b) => if(a <= b) 1 else 0)
      case EQ(e1, e2) => evaluateBinOpCombine(e1, e2, state, (a, b) => if(a == b) 1 else 0)
      case NEQ(e1, e2) => evaluateBinOpCombine(e1, e2, state, (a, b) => if(a != b) 1 else 0)
      case AND(e1, e2) => evaluateBinOpCombine(e1, e2, state, (a, b) => if((a + b) == 2) 1 else 0)
      case OR(e1, e2) => evaluateBinOpCombine(e1, e2, state, (a, b) => if((a + b) != 0) 1 else 0)
    }
  }

  private def evaluateUnOpToConstant(unOp: UnaryOp, state: State): Option[TImm] = {
    unOp match {
      case CHR(e) => evaluateExpressionToConstant(e, state)
      case ORD(e) => evaluateExpressionToConstant(e, state)
      case LEN(Ident(name)) => state.curLocTable.getIdentConstant(s"len($name)") // Only consider for the outer most array
      case LEN(_) => None
      case NEG(e) => {
        evaluateExpressionToConstant(e, state) match {
          case None => None
          case Some(v) => Some(new TImm(-v.value))
        }
      }
      case NOT(e) => {
        evaluateExpressionToConstant(e, state) match {
          case None => None
          case Some(v) => Some(new TImm(if (v.value == 0) 1 else 0))
        }
      }
    }
  }
  
  // recurisevly translate the inner array
  private def translateInnerArray(retLoc: TLoc, exprs: List[Expr], arrayPointerLoc: TLoc, isCharArrElem: Boolean, isPtrArrElem: Boolean, state: State): (TLoc, List[ThreeAddrInstr]) = {
    exprs match {
      case expr :: next => {
        val (indexOp, indexInstrs) = translateExpression(expr, state)
        var instrs: List[ThreeAddrInstr] = {
          if (isCharArrElem) indexInstrs ::: List(TCharArrElemLoad(retLoc, arrayPointerLoc, indexOp))
          else if (isPtrArrElem) indexInstrs ::: List(TPtrArrElemLoad(retLoc, arrayPointerLoc, indexOp))
          else indexInstrs ::: List(TArrElemLoad(retLoc, arrayPointerLoc, indexOp))
        }
        translateInnerArray(retLoc, next, retLoc, isCharArrElem, isPtrArrElem, state) match {
          case (loc, instrs2) => (loc, instrs ++ instrs2)
        }
      }
      case immutable.Nil => (retLoc, List())
    }
  }
  private def translateArrayElem(ident: Ident, exprs: List[Expr], state: State): (TLoc, List[ThreeAddrInstr]) = {
    val loc = state.curLocTable.getNextTRegLoc()

    val arrayLoc = state.curLocTable.lookUpIdent(ident.name) // memory address of array
    val isPtr = state.curSymbolTree.lookUp(ident.name) match {
      case Types.ArrayType(Types.ArrayType(_)) => true 
      case Types.ArrayType(Types.PairType(_, _)) => true
      case _ => false
    }
    val res = translateInnerArray(loc, exprs, arrayLoc, state.curSymbolTree.lookUp(ident.name) == Types.ArrayType(Types.CharType), isPtr, state)
    res
  }

  // translate unary operations
  private def translateUnOp(unOp: UnaryOp, state: State): (TLoc, List[ThreeAddrInstr]) = {    
    val (exprLoc, instrs) = translateExpression(unOp.expr, state)
    val operation = unOp match {
      case NOT(_) => new TNOT(exprLoc, exprLoc)
      case NEG(_) => new TNEG(exprLoc, exprLoc)
      case ORD(_) => new TORD(exprLoc, exprLoc)
      case CHR(_) => new TCHR(exprLoc, exprLoc)
      case LEN(_) =>  new TLEN(exprLoc, exprLoc)
    }
    val newInstrs = instrs ++ List(operation)
    
    (exprLoc, newInstrs)
  }

  // translate binary operations
  private def translateBinOp(binOp: BinaryOp, state: State): (TLoc, List[ThreeAddrInstr]) = {
    // If lExpr translation returns an immeadiate, assign it to a new Loc
    val (lLoc, lInstrs) = translateExpression(binOp.lExpr, state)
    val (rLoc, rInstrs) = translateExpression(binOp.rExpr, state)

    val operation = binOp match {
      case ADD(_, _) => List(new TADD(lLoc, lLoc, rLoc))
      case SUB(_, _) => List(new TSUB(lLoc, lLoc, rLoc))
      case MUL(_, _) => List(new TMUL(lLoc, lLoc, rLoc))
      case DIV(_, _) => List(new TDIV(lLoc, lLoc, rLoc))
      case GT(_, _)  => List(new TGT(lLoc, lLoc, rLoc))
      case GTE(_, _) => List(new TGTE(lLoc, lLoc, rLoc))
      case LT(_, _)  => List(new TLT(lLoc, lLoc, rLoc))
      case LTE(_, _) => List(new TLTE(lLoc, lLoc, rLoc))
      case EQ(_, _)  => List(new TEQ(lLoc, lLoc, rLoc))
      case NEQ(_, _) => List(new TNEQ(lLoc, lLoc, rLoc))
      case AND(_, _) => {
        val (resLoc, instrs) = translateBoolOp(lLoc, rLoc, state, true)
        instrs ++ List(TAssignLoc(lLoc, resLoc))
      }
      case OR(_, _)  => {
        val (resLoc, instrs) = translateBoolOp(lLoc, rLoc, state, false)
        instrs ++ List(TAssignLoc(lLoc, resLoc))
      }
      case MOD(_, _) => List(new TMOD(lLoc, lLoc, rLoc)) // new TMOD(lLoc, lLoc, rLoc)
    }
    val instrs = lInstrs ++ rInstrs ++ operation
    (lLoc, instrs)
  }

  // translate boolean operations (AND, OR) so that it returns early
  private def translateBoolOp(
    lLoc: TLoc, 
    rloc: TLoc, 
    state: State, 
    isAnd: Boolean): (TLoc, List[ThreeAddrInstr]) = {
    val thenBodyLabel = state.getNextTBranchLabel()
    val endLabel = state.getNextTBranchLabel()
    val resultReg = state.curLocTable.getNextTRegLoc()

    val thenBody = {
      val thenResLoc: TLoc = if (isAnd) rloc else lLoc
      List(TAssignLoc(resultReg, thenResLoc))
    }
    val elseBody = {
      val elseResLoc: TLoc = if (isAnd) lLoc else rloc
      List(TAssignLoc(resultReg, elseResLoc))
    }

    (resultReg, List(
      TJumpCond(lLoc, thenBodyLabel),
    ) ++ elseBody ++ List(
      TJump(endLabel),
      thenBodyLabel 
    ) ++ thenBody ++ List (
      endLabel
    ))
  }
}