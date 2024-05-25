package wacc.ir

import wacc.ast.ast._
import wacc.ast.Types._
import expressionTranslator._
import threeAddrCode._
import scala.collection.mutable.ListBuffer

object statementTranslator {
  // Switch for using constant propagation and folding optimization
  val constantOptimization = false

  // entry point to translate main into a ThreeAddrFunc
  def translateMain(stmt: List[Stmt], state: State): ThreeAddrFunc = {
    state.moveScopeDown()
    val instrs = stmt.map(translateStatement(_, state)).flatten

    println(instrs)
    
    val usedArgs = 0 // main function has no arguments
    val usedLocs = state.locsUsed // number of used locs must be retrieved before moving scope up
    state.moveScopeUp()
    new ThreeAddrFunc("main", instrs, usedArgs, usedLocs)
  }

  //  Converts a given Type into an integer representation.
  def typeToInt(paramType: Type): Int = paramType match {
     case IntType => 0
     case BoolType => 1
     case CharType => 2
     case StringType => 3
     case ArrayType(t) => 4
     case PairType(t1, t2) => 5
     case ErasedPairType => 5
     case _ => throw new IllegalArgumentException("Invalid Type")
  }

  // entry point for translating each function to ThreeAddrFuncs
  def translateFunctions(funcs: List[Func], state: State): List[ThreeAddrFunc] = {    
    funcs.map(func => translateFunction(func, state))
  }

  private def translateFunction(func: Func, state: State): ThreeAddrFunc = {
    val Func(_, Ident(funcName), param_list, stmts) = func

    val paramString = param_list.map(param => typeToInt(param.paramType)).mkString("")

    val uniqueFuncName = s"${paramString}_${funcName}"
    state.moveScopeDown()
    // Move arguments to actual Loc and assciate that loc to the param ident
    param_list.zipWithIndex.foreach { 
      case (param, index) => {
        val Param(_, Ident(paramName)) = param
        // Add function arguments to current scope
        state.curLocTable.addIdent(param.ident.name, TArgLoc(index))
      }
    }
    // Translate the statement and add that to the instrs
    val instrs = (statementTranslator.translateStatements(stmts, state))

    val argNum: Int = param_list.length
    val usedLocs: Int = state.locsUsed
    // move scope up after retrieving the usedLocs
    state.moveScopeUp()

    new ThreeAddrFunc(uniqueFuncName, instrs, argNum, usedLocs)
  }

  private def translateStatements(stmt: List[Stmt], state: State): List[ThreeAddrInstr] = {
    state.moveScopeDown()
    val list = stmt.map(translateStatement(_, state)).flatten
    state.moveScopeUp()
    list
  }

  // Translate a statement to a list of three address instructions
  private def translateStatement(stmt: Stmt, state: State): List[ThreeAddrInstr] = {
    stmt match {
      case Skip() => translateSkip()
      case Assign(lvalue, rvalue) => translateAssign(lvalue, rvalue, state)
      case Read(lvalue) => translateRead(lvalue, state)
      case Free(expr) => translateFree(expr, state)
      case Return(expr) => translateReturn(expr, state)
      case Exit(expr) => translateExit(expr, state)
      case Print(expr) => translatePrint(expr, state, newLine = false)
      case Println(expr) => translatePrint(expr, state, newLine = true)
      case IfStmt(expr, thenStmt, elseStmt) => translateIf(expr, thenStmt, elseStmt, state)
      case WhileStmt(expr, stmts) => translateWhile(expr, stmts, state)
      case Scope(stmts) => translateStatements(stmts, state)
      case Declare(identType, ident, rvalue) => translateDeclare(identType, ident, rvalue, state)
    }
  }

  // Skip is a no-op
  private def translateSkip(): List[ThreeAddrInstr] = List()

  // Translate an assignment to a list of three address instructions
  private def translateAssign(lvalue: lValue, rvalue: rValue, state: State): List[ThreeAddrInstr] = {
    var constantVal: Option[TImm] = None
    if (constantOptimization) {
      constantVal = rvalue match {
        case e: Expr => evaluateExpressionToConstant(e, state) // Can try to fold to constant
        case _ => None
      }
    }

    var isIdent = false
    var identName = ""

    val res = lvalue match {
      case Ident(name) => {
        identName = name
        isIdent = true

        constantVal match {
          case None => {
            val (lLoc, linstrs) = (state.curLocTable.lookUpIdent(name), List()) 
            state.enterLocScope()
            val (rLoc, rinstrs) = getRValueLoc(rvalue, state)
            state.exitLocScope()
            linstrs ::: rinstrs ::: List(TAssignLoc(lLoc, rLoc))
          }
          case value: Some[TImm] => {
            List()
          }
        }
      }
      case ArrayElem(Ident(name), exprs) => {
        val ((lLoc, offset), linstrs) = getArrayElemLocOffset(name, exprs, state)
        state.enterLocScope()
        val (rLoc, rinstrs) = constantVal match {
          case None => getRValueLoc(rvalue, state)
          case Some(value) => {
            val rl = state.curLocTable.getNextTRegLoc()
            (rl, List(TAssignImm(rl, value)))
          }
        }
        state.exitLocScope()
        // val (rLoc, rinstrs) = getRValueLoc(rvalue, state)
        val instrs = {
          state.curSymbolTree.lookUp(name) match {
            case ArrayType(CharType) => List(TCharArrElemStore(rLoc, lLoc, offset))
            case ArrayType(ArrayType(_)) => List(TPtrArrElemStore(rLoc, lLoc, offset))
            case ArrayType(PairType(_, _)) => List(TPtrArrElemStore(rLoc, lLoc, offset))
            case _ => List(TArrElemStore(rLoc, lLoc, offset))
          }
        }
        linstrs ::: rinstrs ::: instrs
      }
      case lv: PairElem => {
        val tempLoc = state.curLocTable.getNextTRegLoc()
        val ((lLoc, offset), linstrs) = getPairElemLocOffset(lv, state)
        state.enterLocScope()
        val (rLoc, rinstrs) = constantVal match {
          case None => getRValueLoc(rvalue, state)
          case Some(value) => {
            val rl = state.curLocTable.getNextTRegLoc()
            (rl, List(TAssignImm(rl, value)))
          }
        }
        state.exitLocScope()
        // val (rLoc, rinstrs) = getRValueLoc(rvalue, state)
        linstrs ::: rinstrs ::: List(TPairElemStore(rLoc, lLoc, offset))
      }
    }
    
    if(isIdent) {
      // Add to the Constant Value map
      state.curLocTable.setIdentConstant(identName, constantVal)
    }

    res
  }

  // given an array element (ex. a[1][2]), return the corresponding location and offset
  private def getArrayElemLocOffset(identName: String, exprs: List[Expr], state: State): ((TLoc, TLoc), List[ThreeAddrInstr]) = {
    exprs match {
      case List() => {
        throw new Exception("ArrayElem should have at least one expression")
      }
      case List(expr) => {
        val loc = state.curLocTable.getNextTRegLoc()
        val (offset, exprInstrs) = translateExpression(expr, state)
        ((loc, offset), exprInstrs ::: List(TAssignLoc(loc, state.curLocTable.lookUpIdent(identName))))
      }
      case _ => {
        val ((loc, offset), instrs) = getArrayElemLocOffset(identName, exprs.init, state)
        val (offsetLoc, exprInstrs) = translateExpression(exprs.last, state)
        state.curSymbolTree.lookUp(identName) match {
          case CharType => ((loc, offsetLoc), instrs ::: List(TCharArrElemLoad(loc, loc, offset)) ::: exprInstrs)
          case ArrayType(_) => ((loc, offsetLoc), instrs ::: List(TPtrArrElemLoad(loc, loc, offset)) ::: exprInstrs)
          case PairType(_, _) => ((loc, offsetLoc), instrs ::: List(TPtrArrElemLoad(loc, loc, offset)) ::: exprInstrs)
          case _ => ((loc, offsetLoc), instrs ::: List(TLoad(loc, loc, offset)) ::: exprInstrs)
        }
      }
    }
  }

  // Translate an rvalue to a list fof three address instructions and the location of the result
  private def getRValueLoc(rvalue: rValue, state: State): (TLoc, List[ThreeAddrInstr]) = {
    val res = rvalue match {
      case expr: Expr => translateExpression(expr, state)
      case ArrayLiter(value) => translateArrayLiter(value, state)
      case NewPair(expr1, expr2) => translateNewPair(expr1, expr2, state)
      case Call(Ident(funcName), ArgList(args)) => translateCall(funcName, args, state)
      case pairElem: PairElem => {
        val ((resLoc, offset), instrs) = getPairElemLocOffset(pairElem, state)
        (resLoc, instrs ::: List(TPairElemLoad(resLoc, resLoc, offset)))
      }
    }
    res
  }

  // Translates expression to integer representation
  private def exprToInt(expr: Expr, state: State): Int = {
    val exprType = getExprType(expr, state)
    typeToInt(exprType)
  }

  // Translate a call to a list of three address instructions and the location of the result
  private def translateCall(funcName: String, argExprs: List[Expr], state: State): (TLoc, List[ThreeAddrInstr]) = {
    val resLoc = state.curLocTable.getNextTRegLoc()

    state.enterLocScope()
    val instrs: ListBuffer[ThreeAddrInstr] = ListBuffer()
    val argLocList: ListBuffer[TLoc] = ListBuffer()
    for (argExpr <- argExprs) {
      val argLoc = state.curLocTable.getNextTRegLoc()

      state.enterLocScope()
      val (exprLoc, exprInstr) = translateExpression(argExpr, state)
      state.exitLocScope()

      instrs.appendAll(exprInstr)
      instrs.append(TAssignLoc(argLoc, exprLoc))
      argLocList.append(argLoc)
      
    }

    state.exitLocScope()

    val paramString = argExprs.map(expr => exprToInt(expr, state)).mkString("")
    val uniqueFuncName = s"${paramString}_${funcName}"

    (resLoc, instrs.toList:::List(TCall(uniqueFuncName, argLocList.toList, resLoc)))
  }

  // Translates an array literal into three-address code
  private def translateArrayLiter(exprs: List[Expr], state: State): (TLoc, List[ThreeAddrInstr]) = {
    var isChar = exprs.headOption match {
      case Some(expr) => getExprType(expr, state) match {
        case CharType => true
        case _ => false
      }
      case None => false
    }

    var isPtr = exprs.headOption match {
      case Some(expr) => getExprType(expr, state) match {
        case ArrayType(_) => true
        case PairType(_, _) => true
        case _ => false
      }
      case None => false
    }

    var instrs: ListBuffer[ThreeAddrInstr] = ListBuffer()

    state.enterLocScope()
    val resLoc = state.curLocTable.getNextTRegLoc() // the location where array pointer is stored

    if (isChar) instrs.append(TMallocCharArray(resLoc, exprs.size))
    else if (isPtr) instrs.append(TMallocPtrArray(resLoc, exprs.size))
    else        instrs.append(TMallocArray(resLoc, exprs.size))
    /* TMallocArray should 1) call malloc for (size + 1)
                           2) store the pointer at resLoc
                           3) add (data size) to resLoc, in order to start from 0
                           4) store the len(array) at (arrLoc value - data size) 
                           
       Ex)  mov r0, #16
            bl _malloc
		        mov r12, r0
		        add r12, r12, #4    */

    var index = 0
    val tempLoc = state.curLocTable.getNextTRegLoc()
    for (expr <- exprs) {
      state.enterLocScope()
      
      val (elemLoc, elemInstrs) = translateExpression(expr, state)
      instrs.appendAll(elemInstrs)                                // evaluate the expression and put in elemLoc
      instrs.append(TAssignImm(tempLoc, new TImm(index)))         // put index in tempLoc

      // store elemLoc value in memory
      if (isChar)   instrs.append(TCharStore(elemLoc, resLoc, tempLoc))
      else if (isPtr) instrs.append(TPtrStore(elemLoc, resLoc, tempLoc))
      else          instrs.append(TStore(elemLoc, resLoc, tempLoc))    

      state.exitLocScope()
      index += 1
    }

    state.exitLocScope()

    (resLoc, instrs.toList)
  }

  // Translate a new pair declaration to a list of ThreeAddrInstr and its declared location
  private def translateNewPair(expr1: Expr, expr2: Expr, state: State): (TLoc, List[ThreeAddrInstr]) = {
    val resLoc = state.curLocTable.getNextTRegLoc()
    
    state.enterLocScope()
    val (fstLoc, instr1) = translateExpression(expr1, state)

    val (sndLoc, instr2) = translateExpression(expr2, state)
    
    val tempLoc = state.curLocTable.getNextTRegLoc()
    val instr = List(TMallocPair(resLoc)) ::: instr1 ::: 
      List(TAssignImm(tempLoc, new TImm(0)), TStore(fstLoc, resLoc, tempLoc)) ::: instr2 ::: 
      List(TAssignImm(tempLoc, new TImm(1)), TStore(sndLoc, resLoc, tempLoc))
    state.exitLocScope()
    (resLoc, instr)
  }

  // Determines the location and offset of a pair element
  private def getPairElemLocOffset(pairElem: PairElem, state: State): ((TLoc, TLoc), List[ThreeAddrInstr]) = {
    pairElem match {
      case Fst(Ident(name)) => {
        val resLoc = state.curLocTable.getNextTRegLoc()
        val offsetLoc = state.curLocTable.getNextTRegLoc()
        ((resLoc, offsetLoc), List(TAssignLoc(resLoc, state.curLocTable.lookUpIdent(name)), TAssignImm(offsetLoc, new TImm(0))))
      }
      case Snd(Ident(name)) => {
        val resLoc = state.curLocTable.getNextTRegLoc()
        val offsetLoc = state.curLocTable.getNextTRegLoc()
        ((resLoc, offsetLoc), List(TAssignLoc(resLoc, state.curLocTable.lookUpIdent(name)), TAssignImm(offsetLoc, new TImm(1))))
      }
      case Fst(ArrayElem(Ident(name), exprs)) => {
        val ((resLoc, offsetLoc), instrs) = getArrayElemLocOffset(name, exprs, state) 
        ((resLoc, offsetLoc), instrs ++ List(TArrElemLoad(resLoc, resLoc, offsetLoc), TAssignImm(offsetLoc, new TImm(0))))
      }
      case Snd(ArrayElem(Ident(name), exprs)) => {
        val ((resLoc, offsetLoc), instrs) = getArrayElemLocOffset(name, exprs, state)
        ((resLoc, offsetLoc), instrs ++ List(TArrElemLoad(resLoc, resLoc, offsetLoc), TAssignImm(offsetLoc, new TImm(1))))
      }
      case Fst(lv: PairElem) => {
        val ((resLoc, offsetLoc), instrs) = getPairElemLocOffset(lv, state)
        ((resLoc, offsetLoc), instrs ++ List(TPairElemLoad(resLoc, resLoc, offsetLoc), TAssignImm(offsetLoc, new TImm(0))))
      }
      case Snd(lv: PairElem) => {
        val ((resLoc, offsetLoc), instrs) = getPairElemLocOffset(lv, state)
        ((resLoc, offsetLoc), instrs ++ List(TPairElemLoad(resLoc, resLoc, offsetLoc), TAssignImm(offsetLoc, new TImm(1))))
      }
    }
    
  }

  // Translate an expression to a list of three address instructions and the location of the result
  private def translateRead(lvalue: lValue, state: State): List[ThreeAddrInstr] = {   
    if (constantOptimization) {
      lvalue match {
        // the variable is not a constant after read
        case Ident(name) => state.curLocTable.setIdentConstant(name, None) 
        case _ => 
      }
    }

    val res = lvalue match {
      case Ident(name) => {
        val loc = state.curLocTable.lookUpIdent(name)
        getLValueType(lvalue, state) match {
          case IntType => List(TReadI(loc))
          case CharType => List(TReadC(loc))
          case _ => throw new RuntimeException("Attempt to read from non-readable type")
        }
      }
      case ArrayElem(Ident(name), exprs) => {
        val ((loc, offset), instrs) = getArrayElemLocOffset(name, exprs, state)
        val readLoc = state.curLocTable.getNextTRegLoc()
        getLValueType(lvalue, state) match {
          case IntType => instrs ::: List(TArrElemLoad(readLoc, loc, offset), TReadI(readLoc), TArrElemStore(readLoc, loc, offset))
          case CharType => instrs ::: List(TCharArrElemLoad(readLoc, loc, offset), TReadC(readLoc), TCharArrElemStore(readLoc, loc, offset))
          case _ => throw new RuntimeException("Attempt to read from non-readable type")
        }
      }
      case pairElem: PairElem => {
        val ((loc, offset), instrs) = getPairElemLocOffset(pairElem, state)
        val readLoc = state.curLocTable.getNextTRegLoc()
        getLValueType(lvalue, state) match {
          case IntType => instrs ::: List(TPairElemLoad(readLoc, loc, offset), TReadI(readLoc), TPairElemStore(readLoc, loc, offset))
          case CharType => instrs ::: List(TPairElemLoad(readLoc, loc, offset), TReadC(readLoc), TPairElemStore(readLoc, loc, offset))
          case _ => throw new RuntimeException("Attempt to read from non-readable type")
        }
      }
    }
    res
  }

  private def exprToIdentName(expr: Expr): Option[String] = {
    expr match {
      case Ident(name) => Some(name)
      case _ => None
    }
  }

  private def translateFree(expr: Expr, state: State): List[ThreeAddrInstr] = {
    val (loc, instrs) = translateExpression(expr, state)

    getExprType(expr, state) match {
      case ArrayType(_) => {
        if(constantOptimization) {
          // Remove the length constant value from the IdentConstant
          exprToIdentName(expr) match {
            case Some(name) => state.curLocTable.setIdentConstant(s"len($name)", None)
            case _ => 
          }
        }
        instrs ::: List(TFreeA(loc))
      }
      case PairType(_, _) => instrs ::: List(TFreeP(loc))
      case _ => throw new RuntimeException("Attempt to free non-freeable type")
    }
  }

  private def translateReturn(expr: Expr, state: State): List[ThreeAddrInstr] = {
    val (op, instrs) = if (constantOptimization) getExpressionAsConstantLoc(expr, state) else translateExpression(expr, state)
    // val (op, instrs) = translateExpression(expr, state)
    instrs ::: List(TReturn(op))
  }

  private def translateExit(expr: Expr, state: State): List[ThreeAddrInstr] = {
    val (op, instrs) = if (constantOptimization) getExpressionAsConstantLoc(expr, state) else translateExpression(expr, state)
    // val (op, instrs) = translateExpression(expr, state)
    instrs ::: List(TExit(op))
  }

  private def translatePrint(expr: Expr, state: State, newLine: Boolean): List[ThreeAddrInstr] = {
    val (op, instrs) = if (constantOptimization) getExpressionAsConstantLoc(expr, state) else translateExpression(expr, state)
    // val (op, instrs) = translateExpression(expr, state)    
    val printInstrs = getExprType(expr, state) match {
      case IntType => TPrintI(op, newLine)
      case CharType => TPrintC(op, newLine)
      case BoolType => TPrintB(op, newLine)
      case StringType => TPrintS(op, newLine)
      case ArrayType(CharType) => TPrintS(op, newLine)
      case ArrayType(_) => TPrintP(op, newLine)
      case PairType(_, _) => TPrintP(op, newLine)
      case ErasedPairType => TPrintP(op, newLine)
      case x => throw new RuntimeException("Attempt to print non-printable type")
    }
    instrs ::: List(printInstrs)
  }

  // Translates an if statement to ThreeAddrInstr using branch labels
  private def translateIf(expr: Expr, thenStmt: List[Stmt], elseStmt: List[Stmt], state: State): List[ThreeAddrInstr] = {
    val thenBodyLabel = state.getNextTBranchLabel()
    val endLabel = state.getNextTBranchLabel()

    val (condLoc, condInstr) = translateExpression(expr, state)
    val thenBody = translateStatements(thenStmt, state)
    val elseBody = translateStatements(elseStmt, state)

    condInstr ++ List(
      TJumpCond(condLoc, thenBodyLabel),
    ) ++ elseBody ++ List(
      TJump(endLabel),
      thenBodyLabel
    ) ++ thenBody ++ List (
      endLabel
    )
  }

  // Translates a while loop to ThreeAddrInstr using branch labels
  private def translateWhile(expr: Expr, stmts: List[Stmt], state: State): List[ThreeAddrInstr] = {
    val startBodyLabel = state.getNextTBranchLabel()
    val condCheckLabel = state.getNextTBranchLabel()

    val (condLoc, condInstr) = translateExpression(expr, state)
    val whileBody = translateStatements(stmts, state)

    List(
      TJump(condCheckLabel), // Jump to condition check
      startBodyLabel // Label for start of loop 
    ) ++ 
    whileBody ++ 
    List(condCheckLabel) ++ // Label for condition check
    condInstr ++ 
    List(TJumpCond(condLoc, startBodyLabel)) // Jump to start of loop if condition is true
  }

  // Translates a declare statement to ThreeAddrInstr
  private def translateDeclare(identType: Type, ident: Ident, rvalue: rValue, state: State): List[ThreeAddrInstr] = {
    var constantVal: Option[TImm] = None
    if (constantOptimization) {
      constantVal = rvalue match {
        case e: Expr => evaluateExpressionToConstant(e, state) // Can try to fold to constant
        case _ => None
      }
      state.curLocTable.setIdentConstant(ident.name, constantVal)
    }

    constantVal match {
      case None => {
        // If it cannot be folded into a constant, do normal operation
        val newLoc = state.curLocTable.getNextTRegLoc()
        state.curLocTable.addIdent(ident.name, newLoc)

        state.enterLocScope()
        val (loc, instrs) = getRValueLoc(rvalue, state)
        state.exitLocScope()
        instrs ::: List(TAssignLoc(newLoc, loc))
      }
      case Some(value) => {
        // List(TAssignImm(newLoc, value))
        List()
      }
    }

    // val (loc, instrs) = getRValueLoc(rvalue, state)


    // Add to the Constant Value map
    // state.curLocTable.setIdentConstant(ident.name, constantVal)
    
    // res
    // instrs ::: List(TAssignLoc(newLoc, loc))
  }

  // Determines whether the lvalue is a int or char for undeclared immeadiate values
  private def getLValueType(lvalue: lValue, state: State): Type = {
    lvalue match {
      case expr: Expr => getExprType(expr, state)
      case Fst(lvalue) => getLValueType(lvalue, state) match {
        case PairType(fst, _) => fst
        case _ => throw new RuntimeException("Type is unknown")
      }
      case Snd(lvalue) => getLValueType(lvalue, state) match {
        case PairType(_, snd) => snd
        case _ => throw new RuntimeException("Type is unknown")
      }
    }
  }

  // Determines the type of an expression
  def getExprType(expr: Expr, state: State): Type = {
    expr match {
      case ident: Ident => state.curSymbolTree.lookUp(ident.name)
      case IntLiter(value) => IntType
      case BoolLiter(value) => BoolType
      case CharLiter(value) => CharType
      case StrLiter(value) => StringType
      case ArrayElem(ident, args) => state.curSymbolTree.lookUp(ident.name) match {
        case ArrayType(t) => getInnerArrayType(t, args.length)
        case _ => throw new RuntimeException("Not an array")
      }
      case PairLiter() => ErasedPairType
      case Bracket(expr) => getExprType(expr, state) 
      case binOp: BinaryOp => binOp match {
        case ADD(_, _) | SUB(_, _) | MUL(_, _) | DIV(_, _) | MOD(_, _) => IntType
        case _ => BoolType
      }
      case unOp: UnaryOp => unOp match {
        case NOT(_) => BoolType
        case CHR(_) => CharType
        case _ => IntType
      } 
    }
  }
  // Recursively gets the inner type of an array type
  def getInnerArrayType(t: Type, n: Int): Type = {
    if (n <= 0) t
    else t match {
      case ArrayType(inner) => getInnerArrayType(inner, n - 1)
      case _ => t
    }
}
}
