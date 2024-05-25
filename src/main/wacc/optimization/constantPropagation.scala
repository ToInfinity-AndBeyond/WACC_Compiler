package wacc.optimization

import wacc.ast.Types._
import wacc.ast.ast._
import wacc.errors.WaccError._
import wacc.semantic.SymbolTree
import wacc.optimization.ConstantState
import constantFolding.expressionToConstant

import scala.collection.mutable
import wacc.ir.expressionTranslator
import wacc.syntax.lexer

object constantPropagation {
  private final val p = (0, 0)

  /* Optimize function
     Assume that the parameters are declared and are not constant */
  def optimizeFunction(func: Func, state: ConstantState): Func = {
    val args = func.param_list
    val body = func.stmts

    println("Given AST")
    println(body)

    state.enterScope()
    state.enterScope()

    for (Param(_, Ident(argName)) <- args) {
      state.setIdentAsConstant(argName, None)
      state.setIdentDeclared(argName, true)
    }

    // state.curConstantMap.printConstantMap()
    val optimizedBody = body.map(optimizeStatement(_, state)).flatten
    state.exitScope()
    state.exitScope()

    println("Optimized AST")
    println(optimizedBody)

    Func(func.funcType, func.ident, args, optimizedBody)(p)
  }

  /* Optimize Main body */
  def optimizeMain(stmts: List[Stmt], state: ConstantState): List[Stmt] = {
    println("Given AST")
    println(stmts)

    state.enterScope()
    val optimizedMainBody = stmts.map(optimizeStatement(_, state)).flatten
    state.exitScope()

    println("Optimized AST")
    println(optimizedMainBody)

    optimizedMainBody
  }

  /* Optimize a single statement */
  private def optimizeStatement(stmt: Stmt, state: ConstantState, isScope: Boolean = false, isWhile: Boolean = false): List[Stmt] = {
    // println(stmt)
    // println(state.printConstantMap())
    stmt match {
     case assignStmt: Assign => optimizeAssign(assignStmt, state, isScope, isWhile)
     case declareStmt: Declare => optimizeDeclare(declareStmt, state)
     case Exit(e) => optimizeExit(e, state)
     case Return(e) => optimizeReturn(e, state)
     case Print(e) => optimizePrint(e, state)
     case Println(e) => optimizePrintln(e, state)
     case Read(lv) => optimizeRead(lv, state)
     case IfStmt(ifExpr, thenStmt, elseStmt) => optimizeIf(ifExpr, thenStmt, elseStmt, state)
     case Scope(stmts) => optimizeScope(stmts, state)
     case WhileStmt(whileExpr, bodyStmt) => optimizeWhile(whileExpr, bodyStmt, state)
     case other => List(other)
    }
  }

  /* Optimize statements within a scope
     (if, while, scope, function, main body) */
  private def optimizeStatements(stmts: List[Stmt], state: ConstantState, isScope: Boolean = false, isWhile: Boolean = false): List[Stmt] = {
    state.enterScope()
    val res = stmts.map(optimizeStatement(_, state, isScope, isWhile)).flatten
    state.exitScope()
    res
  }

  /* Optimize scope (begin ... end) */
  private def optimizeScope(stmts: List[Stmt], state: ConstantState): List[Stmt] = {
    List(Scope(optimizeStatements(stmts, state, true))(p))
  }

  /* Optimize assign
     If it is a scope, change the constant value mapping
     If it is a while body, keep the assign statement (as it can be called again for indefinite time) */
  private def optimizeAssign(assignStmt: Assign, state: ConstantState, isScope: Boolean = false, isWhile: Boolean = false): List[Stmt] = {
    if(isWhile)
      // Assign statement in While cannot be optimized
      // Simulating the while is defeats the purpose of compiling
      return List(assignStmt)

    assignStmt.lvalue match {
      case Ident(name) => {
        assignStmt.rvalue match {
          case e: Expr => {
            val constVal = expressionToConstant(e, state)
            val position = assignStmt.pos
            constVal match {
              case Some(value) => {
                if (value > Int.MaxValue) 
                  throw new RuntimeException(s"Integer Overflow: trying to assign $constVal to $name $position")
                if (value < Int.MinValue)
                  throw new RuntimeException(s"Integer Underflow: trying to assign $constVal to $name $position")
              }
              case None => 
            }
            
            if (isScope) {
              // println(s"set constant $name as $constVal")
              state.setIdentAsConstantOverall(name, constVal)
              // If it is an ident from outside, dont assign it
              // If it is a while statement, the value should be reassigned, as it maybe used in the expression
              if (!state.hasLocalIdent(name)) 
                return List()
            }

            state.setIdentAsConstant(name, constVal)

            constVal match {
              case Some(value) => {
                if (isScope) {
                  getExpressionType(e, state) match {
                    case IntType => List(Assign(Ident(name)(p), IntLiter(value)(p))(p))
                    case BoolType => List(Assign(Ident(name)(p), BoolLiter(if (value == 0) false else true)(p))(p))
                    case CharType => {
                      if (value >= 128) 
                        throw new RuntimeException(s"Character Overflow: trying to assign $value to $name $position")
                      if (value < 0)
                        throw new RuntimeException(s"Character Underflow: trying to assign $value to $name $position")

                      List(Assign(Ident(name)(p), CharLiter(value.toChar)(p))(p))
                    }
                    case _ => List(assignStmt)
                  }
                }
                else
                  List()
              }
              // If the ident is an expression but no longer a constant, declare it if it was not declared
              case None => {
                if (state.isIdentDeclared(name)) {
                  // If it is declared, don't declare
                  state.setIdentAsConstant(name, None)
                  List(assignStmt)
                }
                else {
                  state.setIdentDeclared(name, true) // If it is not declared and it was a constant, declare
                  state.setIdentAsConstant(name, None)
                  List(Declare(state.getIdentType(name), Ident(name)(p), assignStmt.rvalue)(p))
                }
              }
            }
          }
          case c: Call => {
            val res = if (state.isIdentDeclared(name)) 
              List(Assign(Ident(name)(p), optimizeCall(c, state))(p))
            else {
              state.setIdentDeclared(name, true)
              List(Declare(state.getIdentType(name), Ident(name)(p), optimizeCall(c, state))(p))
            }

            state.setIdentAsConstant(name, None)

            res
          }
          case _ => {
            // If it is assigning to arrayLiteral, pairLiteral it cannot be optimized
            state.setIdentAsConstant(name, None)
            List(assignStmt)
          }
        }
      }
      case _ => List(assignStmt)
    }
  }

  /* Optimize Declare
     Instead of declaring right away, try to put it in a constant map if possible
     Constant includes int, char, and bool */
  private def optimizeDeclare(declareStmt: Declare, state: ConstantState): List[Stmt] = {
    val identName = declareStmt.ident.name
    declareStmt.rvalue match {
      case e: Expr => {
        val constVal = expressionToConstant(e, state)

        state.setIdentAsConstant(identName, constVal)

        constVal match {
          case Some(value) => {
            val position = declareStmt.pos
            if (value > Int.MaxValue) 
              throw new RuntimeException(s"Integer Overflow: trying to assign $constVal to $identName $position")
            if (value < Int.MinValue)
              throw new RuntimeException(s"Integer Underflow: trying to assign $constVal to $identName $position")

            state.setIdentDeclared(identName, false)
            List()
          }
          case None => {
            state.setIdentDeclared(identName, true)
            List(declareStmt)
          }
        }
      }
      case c: Call => {
        val res = List(Declare(declareStmt.identType, declareStmt.ident, optimizeCall(c, state))(p))
        state.setIdentAsConstant(identName, None)
        state.setIdentDeclared(identName, true)
        res 
      }
      case _ => {
        state.setIdentAsConstant(identName, None)
        state.setIdentDeclared(identName, true)
        List(declareStmt)
      }
    }
  }

  /* Optimize function Call
     Evaluate the parameters to constant if possible and then call
     Result of the call is unknown, and simulating the function is beyond the scope of optimization,
     so assume that the result is a non-constant (None) */
  private def optimizeCall(callStmt: Call, state: ConstantState): Call = {
    val ArgList(argExprs) = callStmt.args
    val newArgs: mutable.ListBuffer[Expr] = mutable.ListBuffer()

    for (argExpr <- argExprs) {
      val newArg = expressionToConstant(argExpr, state) match {
        case None => 
          argExpr
        case Some(value) => {
          getExpressionType(argExpr, state) match {
            case IntType => IntLiter(value)(p)
            case BoolType => BoolLiter(if (value==0) false else true)(p)
            case CharType => CharLiter(value.toChar)(p)
            case t => throw new RuntimeException(s"Argument $argExpr is evaluated as constant $value, but is type $t")
          }
        }
      }
      newArgs.addOne(newArg)
    }

    Call(callStmt.ident, ArgList(newArgs.toList)(p))(p)
  }

  /* Declare the ident which was never declared before as its constant value */
  private def declareConstantToIdent(identName: String, state: ConstantState): List[Stmt] = {
    // Cannot declare if the variable was never given until now
    if (!state.curConstantMap.identExist(identName)) 
      return List()

    // Ignore if it is already declared
    if (state.isIdentDeclared(identName)) 
      return List()

    state.getIdentValue(identName) match {
      case None => throw new RuntimeException(s"$identName should have been declared already, as it is not a constant")
      case Some(value) => {
        state.getIdentType(identName) match {
          case IntType => {
            state.setIdentDeclared(identName, true) 
            List(Declare(IntType, Ident(identName)(p), IntLiter(value)(p))(p))
          }
          case BoolType => {
            if (value == 0) {
              state.setIdentDeclared(identName, true) 
              List(Declare(BoolType, Ident(identName)(p), BoolLiter(false)(p))(p))
            }
            else if (value == 1) {
              state.setIdentDeclared(identName, true) 
              List(Declare(BoolType, Ident(identName)(p), BoolLiter(true)(p))(p))
            }
            else
              throw new RuntimeException(s"Boolean constant value with non 0 or 1 value during optimization: $value")
          }
          case CharType => {
            state.setIdentDeclared(identName, true) 
            List(Declare(CharType, Ident(identName)(p), CharLiter(value.toChar)(p))(p))
          }
          case t => throw new RuntimeException(s"A non Int, Bool, Char type, $t is stored as a constant for $identName")
        }
      }
    }
  }

  /* Reassign the ident to its constant value
     If it was never declared, call declareConstantToIdent */
  private def reassignOrDeclareConstant(identName: String, state: ConstantState): List[Stmt] = {
    if (!state.curSymbolTree.currentScope.contains(identName)) // Cannot declare if it is not in scope
      return List()
      
    state.getIdentValue(identName) match {
      case None => List()
      case Some(value) => {
        if (state.isIdentDeclared(identName)) {
          // Ignore if it is already declared
          state.getIdentType(identName) match {
            case IntType => List(Assign(Ident(identName)(p), IntLiter(value)(p))(p))
            case BoolType => List(Assign(Ident(identName)(p), BoolLiter(if (value==0) false else true)(p))(p))
            case CharType => List(Assign(Ident(identName)(p), CharLiter(value.toChar)(p))(p))
            case t => throw new RuntimeException(s"$identName of type $t is evaluated as constant: $value")
          }
        } else 
          declareConstantToIdent(identName, state)
      }
    }
  }

  /* Optimize Exit. 
     Try to evaluate the expression to constant, and exit with that value if possible */
  private def optimizeExit(exitExpr: Expr, state: ConstantState): List[Stmt] = {
    expressionToConstant(exitExpr, state) match {
      case None => List(Exit(exitExpr)(p))
      case Some(value) => List(Exit(IntLiter(value)(p))(p))
    }
  }

  /* Optimize Return. 
     Try to evaluate the expression to constant, and return with that value if possible */
  private def optimizeReturn(exitExpr: Expr, state: ConstantState): List[Stmt] = {
    expressionToConstant(exitExpr, state) match {
      case None => List(Return(exitExpr)(p))
      case Some(value) => List(Return(IntLiter(value)(p))(p))
    }
  }

  /* Optimize Print
     Optimize the expression to constant and print that if possible */
  private def optimizePrint(printExpr: Expr, state: ConstantState): List[Stmt] = {
    expressionToConstant(printExpr, state) match {
      case None => List(Print(printExpr)(p))
      case Some(value) => {
        getExpressionType(printExpr, state) match {
          case IntType => List(Print(IntLiter(value)(p))(p))
          case BoolType => List(Print(BoolLiter(if (value == 0) false else true)(p))(p))
          case CharType => List(Print(CharLiter(value.toChar)(p))(p))
          case t => {
            throw new RuntimeException(s"Constant cannot be evaluated for non Int, Bool, Char Type ($t) in Print")
          }
        }
      }
    }
  }
  
  private def optimizePrintln(printlnExpr: Expr, state: ConstantState): List[Stmt] = {
    expressionToConstant(printlnExpr, state) match {
      case None => List(Println(printlnExpr)(p))
      case Some(value) => {
        getExpressionType(printlnExpr, state) match {
          case IntType => List(Println(IntLiter(value)(p))(p))
          case BoolType => List(Println(BoolLiter(if (value == 0) false else true)(p))(p))
          case CharType => List(Println(CharLiter(value.toChar)(p))(p))
          case t => {
            throw new RuntimeException(s"Constant cannot be evaluated for non Int, Bool, Char Type ($t) in Println")
          }
        }
      }
    }
  }

  /* Optimize Read statement. The ident it accesses is no longer a constant */
  private def optimizeRead(lv: lValue, state: ConstantState): List[Stmt] = {
    lv match {
      case Ident(name) => {
        val stmts = declareConstantToIdent(name, state)
        state.setIdentAsConstant(name, None)
        stmts ::: List(Read(lv)(p))
      }
      case _ => List(Read(lv)(p))
    }
  }

  /* Get reassigned idents from statements within a scope */
  private def getReassignedIdents(stmts: List[Stmt], state: ConstantState): List[String] = {
    val localDeclaredIdents: mutable.Set[String] = mutable.Set()
    val localAssignedIdents: mutable.Set[String] = mutable.Set()
    for (stmt <- stmts) {
      stmt match {
        case Declare(_, Ident(name), _) => localDeclaredIdents.add(name)
        case Assign(Ident(name), _) => {
          if (!localDeclaredIdents.contains(name)){
            localAssignedIdents.add(name)
          }
        }
        case Read(Ident(name)) => {
          if (!localDeclaredIdents.contains(name)){
            localAssignedIdents.add(name)
          }
        }
        case IfStmt(ifExpr, thenStmt, elseStmt) => {
          localAssignedIdents.addAll(getExprIdents(ifExpr))
          localAssignedIdents.addAll(getReassignedIdents(thenStmt, state))
          localAssignedIdents.addAll(getReassignedIdents(elseStmt, state))
        }
        case WhileStmt(_, bodyStmt) => localAssignedIdents.addAll(getReassignedIdents(bodyStmt, state))
        case _ => 
      }
    }

    localAssignedIdents.toList
  }

  /* Traversing function to match the scope
     Go through statements, enter and exit scope accordingly */
  private def traverseDeadStatement(stmts: List[Stmt], state: ConstantState): Unit = {
    state.enterScope()
    for (stmt <- stmts) {
      stmt match {
        case IfStmt(_, thenStmt, elseStmt) => {
          // For then
          traverseDeadStatement(thenStmt, state)
          // For else
          traverseDeadStatement(elseStmt, state)
        }
        case WhileStmt(_, stmts) => {
          traverseDeadStatement(stmts, state)
        }
        case Scope(stmts) => {
          traverseDeadStatement(stmts, state)
        }
        case _ => 
      }
    }
    state.exitScope()
  }

  /* Control flow analyzed optimization 
     If the condition is a constant, skip the then body or else body accordingly */
  private def optimizeIf(ifExpr: Expr, thenStmt: List[Stmt], elseStmt: List[Stmt], state: ConstantState): List[Stmt] = {
    expressionToConstant(ifExpr, state) match {
      case Some(1) => {
        val thenOptStmt = optimizeStatements(thenStmt, state, true) 
        traverseDeadStatement(elseStmt, state) // To match the scope
        thenOptStmt
      }
      case Some(0) => {
        traverseDeadStatement(thenStmt, state) // To match the scope
        val elseOptStmt = optimizeStatements(elseStmt, state, true) 
        elseOptStmt
      }
      case Some(x) => throw new RuntimeException(s"$ifExpr is evlautate to a constant value of $x, which is not a boolean (0 or 1)")
      case None => {
        val declareStmts: mutable.ListBuffer[Stmt] = mutable.ListBuffer()
        for (ident <- getExprIdents(ifExpr)) {
          declareStmts.addAll(reassignOrDeclareConstant(ident, state))
        }
        for (ident <- getReassignedIdents(thenStmt, state)) {
          declareStmts.addAll(declareConstantToIdent(ident, state))
          state.setIdentAsConstant(ident, None)
        }
        for (ident <- getReassignedIdents(elseStmt, state)) {
          declareStmts.addAll(declareConstantToIdent(ident, state))
          state.setIdentAsConstant(ident, None)
        }

        val thenOptStmt = optimizeStatements(thenStmt, state, true, true)
        val elseOptStmt = optimizeStatements(elseStmt, state, true, true) 
        declareStmts.toList ::: List(IfStmt(ifExpr, thenOptStmt, elseOptStmt)(p))
      }
    }
  }

  private def optimizeWhile(whileExpr: Expr, bodyStmt: List[Stmt], state: ConstantState): List[Stmt] = {
    expressionToConstant(whileExpr, state) match {
      case Some(0) => {
        // Traverse the dead scope inside the body and don't execute the function
        traverseDeadStatement(bodyStmt, state)
        List()
      }
      case _ => {
        val declareStmts: mutable.ListBuffer[Stmt] = mutable.ListBuffer()
        for (ident <- getExprIdents(whileExpr)) {
          // Declare idents that are used in the while case expression
          declareStmts.addAll(declareConstantToIdent(ident, state))
        }

        for (ident <- getReassignedIdents(bodyStmt, state)) {
          // Declare idents that are used within while body statements
          declareStmts.addAll(reassignOrDeclareConstant(ident, state))
          state.setIdentAsConstant(ident, None)
        }

        val bodyOptStmt = optimizeStatements(bodyStmt, state, true, true)
        declareStmts.toList ::: List(WhileStmt(whileExpr, bodyOptStmt)(p))
      }
    }
  }

  private def getExprIdents(expr: Expr): List[String] = {
    expr match {
      case Ident(name) => List(name)
      case MUL(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case DIV(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case MOD(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case ADD(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case SUB(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case GT(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case GTE(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case LT(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case LTE(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case EQ(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case NEQ(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case AND(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case OR(e1, e2) => getExprIdents(e1) ::: getExprIdents(e2)
      case CHR(e) => getExprIdents(e)
      case ORD(e) => getExprIdents(e)
      case LEN(e) => getExprIdents(e)
      case NEG(e) => getExprIdents(e)
      case NOT(e) => getExprIdents(e)
      case Bracket(e) => getExprIdents(e)
      case _: Atom => List()
    }
  }

  private def getExpressionType(expr: Expr, state: ConstantState): Type = {
    expr match {
      case ident: Ident => state.getIdentType(ident.name)
      case IntLiter(value) => IntType
      case BoolLiter(value) => BoolType
      case CharLiter(value) => CharType
      case StrLiter(value) => StringType
      case ArrayElem(ident, args) => state.getIdentType(ident.name) match {
        case ArrayType(t) => getInnerArrayType(t, args.length)
        case _ => throw new RuntimeException("Not an array")
      }
      case PairLiter() => ErasedPairType
      case Bracket(expr) => getExpressionType(expr, state) 
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

  private def getInnerArrayType(t: Type, n: Int): Type = {
    if (n <= 0) t
    else t match {
      case ArrayType(inner) => getInnerArrayType(inner, n - 1)
      case _ => t
    }
  }
}
