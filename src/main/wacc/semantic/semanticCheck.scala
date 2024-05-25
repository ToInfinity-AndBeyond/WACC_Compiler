package wacc.semantic

import wacc.ast.Types._
import wacc.ast.ast._
import wacc.errors.WaccError._

import TypeCheck._
import scala.annotation.tailrec
import wacc.ir.statementTranslator._
import parsley.state
import wacc.semantic

object SemanticCheck {
  /* 
    All functions in this file are used to check the semantic correctness of the WACC program
      They all return a Either[List[WaccError], Type] (besides the semanticCheck function entry point)
      The Left collects the detected error and the Right returns the type of that expression
  */
  
  // Entry point for the semantic checker
  def semanticCheck(ast: Program): Either[List[WaccError], SymbolTree] = {
    val symbolTable = new SymbolTable(None)
    val errorResult = merge(checkFuncs(ast.funcs, symbolTable), checkStmts(ast.stmts, None, symbolTable)).left.toOption

    errorResult match {
      case None => Right(SymbolTree.freeze(symbolTable, None))
      case Some(err) => Left(err)
    }
  }

  // Add context information to generated error messages
  implicit class EitherOps(a: Either[List[WaccError], Type]) {
    def addContext(msg: String): Either[List[WaccError], Type] = a match {
      case Left(errs) => Left(errs.map(_.addContext(msg)))
      case _ => a
    }
  }

  /* 
    Given two Either[List[WaccError], Type], merge them into one
      If either of them are errors, concat them and return as a combined error
      If both successful, return the first type (as it should be the same)
  */
  def merge(a: Either[List[WaccError], Type], b: Either[List[WaccError], Type]): Either[List[WaccError], Type] = {
    (a, b) match {
      case (Left(err1), Left(err2)) => Left(err1 ::: err2)
      case (Left(err), _)           => Left(err)
      case (_, Left(err))           => Left(err)
      case (Right(t1), Right(t2))   => Right(t1)
    }
  }

  /* 
    Given a Either, runs the second function if the first is successful
      If the first is an error, return that error
      If the second is an error, return that error
      If both successful, return the first returned type       
  */
  def ifThen(a: Either[List[WaccError], Type], b: => Either[List[WaccError], Type]): Either[List[WaccError], Type] = {
    a.flatMap(t => b.flatMap(_ => Right(t))) 
  }

  def generateUniqueIdent(name: String, params: List[Type]): String = {
    val paramsString = params.map(_.toString).mkString(",")
    val uniqueIdent = s"${name}_${paramsString}"
    uniqueIdent
  }

  /* 
    Checks all functions in the program
      For each function, checks the declaration and insert it to the symbol table
      Then checks the statements of each body
  */
  private def checkFuncs(funcs: List[Func], symbolTable: SymbolTable): Either[List[WaccError], Type] = {
    funcs match {
      case Nil => Right(AnyType)
      case _ => {
        // For declaration errors: map the success type to AnyType to align with Type expected by merge
        val decErrs = funcs.map(func => {
          symbolTable.insertFunc(
            func.ident.name,
            func.ident.pos,
            FuncType(func.funcType, func.param_list.map(_.paramType))
          ).map(_ => AnyType: Type) // Align success type with Type for merge
      }).reduce(merge)

        // For body errors: ensure checkFunc is called correctly with each function and the current symbolTable
        val bodyErrs = funcs.map(func => checkFuncBody(func, symbolTable)).reduce(merge)

        merge(decErrs, bodyErrs)
      }
    }
  }

  /* 
    Func Body Check
      Given a function, checks the body code
      Builds a symbol table for the local scope and validates those statements
  */
  private def checkFuncBody(func: Func, symbolTable: SymbolTable): Either[List[WaccError], Type] = {
    val functionScopeSymbolTable = new SymbolTable(Some(symbolTable))

    // Declare parameters in the function's local scope
    val decErrs = func.param_list.foldLeft[Either[List[WaccError], Type]](Right(AnyType)) { (acc, param) =>
      val paramInsertResult = functionScopeSymbolTable.insert(param.ident.name, param.ident.pos, param.paramType)
      // Map the successful insertion to AnyType for merging, as the specific type isn't relevant here.
      val mappedResult = paramInsertResult.map(_ => AnyType: Type)
      merge(acc, mappedResult)
    }.addContext(s"function ${func.ident.name} declaration")

    // Process function body (new symbol table is created inside the CheckStmts)
    val bodyErrs = checkStmts(func.stmts, Some(func.funcType), functionScopeSymbolTable)
                    .addContext(s"function ${func.ident.name} body")

    // Combine declaration errors and body errors
    merge(decErrs, bodyErrs)
  }

  /* 
    Statements Check
      Enters a new scope and checks each statement line by line 
  */
  private def checkStmts(stmts: List[Stmt], returnType: Option[Type], symbolTable: SymbolTable): Either[List[WaccError], Type] = {
    val blockScopeSymbolTable = new SymbolTable(Some(symbolTable))
    // Check the statements using the block's local scope symbol table
    stmts match {
      case Nil => Right(AnyType) // Gracefully handle empty blocks
      case _ => stmts.map(stmt => checkStmt(stmt, returnType, blockScopeSymbolTable)).foldLeft[Either[List[WaccError], Type]](Right(AnyType))(merge)
    }
  }

  /* 
    Each statement type maps to its own check function
      Each case pattern matched by the type to keep position information
  */
  private def checkStmt(stmt: Stmt, returnType: Option[Type], symbolTable: SymbolTable): Either[List[WaccError], Type] = {
    stmt match {
      case print: Print         => checkPrint(print, symbolTable)
      case println: Println     => checkPrintln(println, symbolTable)
      case free: Free           => checkFree(free, symbolTable)
      case ifstmt: IfStmt       => checkIfStmt(ifstmt, returnType, symbolTable)
      case ret: Return          => checkReturn(ret, returnType, symbolTable)
      case dec: Declare         => checkDeclare(dec, symbolTable)
      case scope: Scope         => checkStmts(scope.stmt, returnType, symbolTable)
      case whilestmt: WhileStmt => checkWhileStmt(whilestmt, returnType, symbolTable)
      case exit: Exit           => checkExit(exit, symbolTable)
      case skip: Skip           => checkSkip()
      case asn: Assign          => checkAssign(asn, symbolTable)
      case read: Read           => checkRead(read, symbolTable)
    }
  }

  /* 
    Print and Println statement check
      Both print and println can take any type 
      (hence inferType is used as the identity function for type check)
  */
  private def checkPrint(print: Print, symbolTable: SymbolTable): Either[List[WaccError], Type] =
    checkExpr(print.expr, inferType, symbolTable)
  private def checkPrintln(println: Println, symbolTable: SymbolTable): Either[List[WaccError], Type] =
    checkExpr(println.expr, inferType, symbolTable)

  /* 
    Free statement check
      Free can only be used on arrays and pairs
  */
  private def checkFree(free: Free, symbolTable: SymbolTable): Either[List[WaccError], Type] =
    checkExpr(
      free.expr,
      checkMultiple(canWeakenTo, List(ArrayType(AnyType), PairType(AnyType, AnyType))), 
      symbolTable
    )

  /* 
    Return statement check
      Return only happens inside functions, and must match the return type of the function
  */
  private def checkReturn(ret: Return, returnType: Option[Type], symbolTable: SymbolTable): Either[List[WaccError], Type] =
    returnType match {
      case Some(expected) => checkExpr(ret.expr, canWeakenTo(expected), symbolTable)
      case None           => 
        Left(List(OtherError("Return", None, None, None, Some("Cannot return from main"), ret.pos)))
    }

  /*  
    Declare statement check
      Checks the inferred type of the RValue against the declared type and inserts to the symbol table
      order matters to handle self declaration (e.g. int x = x)
  */
  private def checkDeclare(dec: Declare, symbolTable: SymbolTable): Either[List[WaccError],Type] =
    merge(
      checkRValue(dec.rvalue, canWeakenTo(dec.identType), symbolTable),
      symbolTable.insert(dec.ident.name, dec.ident.pos, dec.identType).map(_ => dec.identType)
    )
  
  /* 
    If statement check
      Checks that the condition is a bool and validates the "then" and "else" clauses
  */
  private def checkIfStmt(ifstmt: IfStmt, returnType: Option[Type], symbolTable: SymbolTable): Either[List[WaccError], Type] =
    merge(
      checkExpr(ifstmt.expr, isExactly(BoolType), symbolTable),
      merge(
        checkStmts(ifstmt.thenStmt, returnType, symbolTable), 
        checkStmts(ifstmt.elseStmt, returnType, symbolTable)
      )
    )

  /* 
    While statement check
      Checks that the condition is a bool and validates the "do" clauses
  */
  private def checkWhileStmt(whilestmt: WhileStmt, returnType: Option[Type], symbolTable: SymbolTable): Either[List[WaccError], Type] =
    merge(
      checkExpr(whilestmt.expr, isExactly(BoolType), symbolTable), 
      checkStmts(whilestmt.stmts, returnType, symbolTable)
    )

  /* 
    Exit statement check
      Checks that it returns an int
  */
  private def checkExit(exit: Exit, symbolTable: SymbolTable): Either[List[WaccError], Type] =
    checkExpr(exit.expr, isExactly(IntType), symbolTable)
  

  // Skip is a no-op
  
  private def checkSkip(): Either[List[WaccError], Type] = Right(AnyType)

  /* 
    Assign statement check
      Checks that the LValue is a valid type and that the RValue can be weakened to it
  */
  private def checkAssign(assign: Assign, symbolTable: SymbolTable): Either[List[WaccError], Type] =
    checkLValue(assign.lvalue, inferType, symbolTable).flatMap(t =>
      checkRValue(assign.rvalue, canWeakenTo(t), symbolTable)
    )

  /* 
    Read statement check
      Checks that the LValue is a valid type
  */
  private def checkRead(read: Read, symbolTable: SymbolTable): Either[List[WaccError], Type] =
    checkLValue(read.lvalue, checkMultiple(isExactly, List(IntType, CharType)), symbolTable)

  /* 
    Expression Check
      Gets the overall type of the expression, checking for any type errors
  */
  private def checkExpr(
    expr: Expr,
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] = {
    expr match {
      case IntLiter(value)  => isType(IntType, expr.pos)
      case BoolLiter(value) => isType(BoolType, expr.pos)
      case CharLiter(value) => isType(CharType, expr.pos)
      case StrLiter(value)  => isType(StringType, expr.pos)
      case PairLiter()      => isType(ErasedPairType, expr.pos)
      case ident: Ident =>
        symbolTable.lookUp(ident.name, ident.pos).flatMap(isType(_, expr.pos))
      // int x = arr[expr1][expr2] -> exprs must be integers
      // get array element, infer type of array and check if index is int
      case ArrayElem(ident, exprs) =>
        symbolTable
          .lookUp(ident.name, ident.pos)
          .flatMap(arr => arr match {
            case _: ArrayType => extractArray(arr, exprs.length) match {
              case Left(_) => 
                Left(List(TypeError(None, None, None, Some("Array dimentions do not match"), ident.pos)))
              case Right(t) =>
                merge(
                  isType(t, expr.pos),
                  exprs.map(checkExpr(_, isExactly(IntType), symbolTable)).reduce(merge).map(_ => t)
                )
            }
            case t => 
              Left(List(TypeError(None, None, None, Some(s"Array extraction is not possible for $t"), ident.pos))
            )
          }
          )
      case Bracket(expr)   => checkExpr(expr, isType, symbolTable)
      case unOp: UnaryOp   => checkUnOp(unOp, isType, symbolTable)
      case binOp: BinaryOp => checkBinOp(binOp, isType, symbolTable)
    }
  }

  // Helper functions for extracting the array type
  @tailrec
  private def extractArray(t: Type, n: Int): Either[List[WaccError], Type] = {
    (t, n) match {
      case (ArrayType(t), 1) => Right(t)
      case (ArrayType(t), n) => extractArray(t, n - 1)
      case _                 => Left(Nil)
    }
  }

  /* 
    Binary operation check
      First check if the binary operation returns the expected type
      Then checks if the left and right expressions can be used against the operation
  */
  private def checkBinOp(
    binOp: BinaryOp,
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] =
    binOp match {
      case GT(_, _) | GTE(_, _) | LT(_, _) | LTE(_, _) =>
        ifThen(
          isType(BoolType, binOp.pos),
          merge(
            checkExpr(
              binOp.lExpr,
              checkMultiple(isExactly, List(IntType, CharType)),
              symbolTable
            ), 
            checkExpr(
              binOp.rExpr,
              checkMultiple(isExactly, List(IntType, CharType)),
              symbolTable
            )
          )
        )
      case ADD(_, _) | SUB(_, _) | MUL(_, _) | DIV(_, _) | MOD(_, _) =>
        ifThen(
          isType(IntType, binOp.pos),
          merge(checkExpr(binOp.lExpr, isExactly(IntType), symbolTable), 
                checkExpr(binOp.rExpr, isExactly(IntType), symbolTable))
        )
      case EQ(_, _) | NEQ(_, _) =>
        ifThen(
          isType(BoolType, binOp.pos),
          checkExpr(binOp.lExpr, inferType, symbolTable)
            // Check if the inferred left type shares anscestor with the inferred right type
            .flatMap(t => checkExpr(binOp.rExpr, shareAncestor(t), symbolTable))
        )
      case OR(_, _) | AND(_, _) =>
        ifThen(
          isType(BoolType, binOp.pos),
          merge(checkExpr(binOp.lExpr, isExactly(BoolType), symbolTable),
                checkExpr(binOp.rExpr, isExactly(BoolType), symbolTable))
        )
    }

  /* 
    Unary operation check
      First check if the unary operation returns the expected type, 
      then checks if the expression can be used against the operation
  */
  private def checkUnOp(
    unOp: UnaryOp,
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] =
    unOp match {
      case NOT(expr) =>
        ifThen(isType(BoolType, unOp.pos), checkExpr(expr, isExactly(BoolType), symbolTable))
      case NEG(expr) =>
        ifThen(isType(IntType, unOp.pos), checkExpr(expr, isExactly(IntType), symbolTable))
      case LEN(expr) =>
        ifThen(isType(IntType, unOp.pos), checkExpr(expr, canWeakenTo(ArrayType(AnyType)), symbolTable))
      case ORD(expr) =>
        ifThen(isType(IntType, unOp.pos), checkExpr(expr, isExactly(CharType), symbolTable))
      case CHR(expr) =>
        ifThen(isType(CharType, unOp.pos), checkExpr(expr, isExactly(IntType), symbolTable))
    }

  /* 
    LValue check
      Either a expression or a pair elem, so passes it on to the relevant checker
  */
  private def checkLValue(
    lvalue: lValue,
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] = {
    lvalue match {
      case expr: Expr         => checkExpr(expr, isType, symbolTable)
      case pairElem: PairElem => checkPairElem(pairElem, isType, symbolTable)
    }
  }

  /* 
    RValue check
      Expression and pair elems are passed to the relevant checker
      ArrayLiter, NewPair and Call are checked against the expected type and validates expressions
  */
  private def checkRValue(
    rvalue: rValue,
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] = {
    rvalue match {
      case expr: Expr             => checkExpr(expr, isType, symbolTable)
      case pairElem: PairElem     => checkPairElem(pairElem, isType, symbolTable)
      case arrayLiter: ArrayLiter => checkArrayLiter(arrayLiter, isType, symbolTable)
      case newPair: NewPair       => checkNewPair(newPair, isType, symbolTable)
      case call: Call             => checkCall(call, isType, symbolTable)

    }
  }

  /* 
    Array Literal Check
      Checks that the array literal matches the expected type
      and that all elements are of the matching same type
  */
  private def checkArrayLiter(
    arr: ArrayLiter, 
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] = {
    val res = arr.value match {
      case Nil => isType(ArrayType(AnyType), arr.pos)
      case head :: tail =>
        tail.foldLeft(checkExpr(head, inferType, symbolTable))((acc, expr) =>
          acc match {
            case Left(err) => Left(err)
            case Right(t)  => checkExpr(expr, shareAncestor(t), symbolTable)
          }
        )
        .flatMap(t => t match {
          case ArrayType(CharType) => isType(ArrayType(t), arr.pos) match {
            case Left(err) => isType(ArrayType(StringType), arr.pos)
            case Right(t) => Right(t)
          } 
          case _ => isType(ArrayType(t), arr.pos)
        })
    }
    res.addContext("array literal declaration")
  }

  /* 
    New Pair Check
    Infers the left and right expressions types, and checks if they match the expected type
  */
  private def checkNewPair(
    arr: NewPair, 
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] = {
    val left = checkExpr(arr.expr1, inferType, symbolTable)
    val right = checkExpr(arr.expr2, inferType, symbolTable)
    val res = (left, right) match {
      case (Right(t1: PairType), Right(t2: PairType)) 
              => isType(PairType(ErasedPairType, ErasedPairType), arr.pos)
      case (Right(t1: PairType), Right(t2: PairElemType))
              => isType(PairType(ErasedPairType, t2), arr.pos)
      case (Right(t1: PairElemType), Right(t2: PairType))
              => isType(PairType(t1, ErasedPairType), arr.pos)
      case (Right(t1: PairElemType), Right(t2: PairElemType))
              => isType(PairType(t1, t2), arr.pos)
      case (Left(err1), Left(err2))
              => Left(err1 ::: err2)
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
      case (Right(t1), Right(t2)) =>
        Left(List(OtherError("unexpected", None, None, None, 
          Some("AnyType and FuncType are not allowed to be used in pair types"), arr.pos)))
    }
    res.addContext("newpair declaration")
  }

  // Determines the type of an expression
  def ExprToType(expr: Expr, symbolTable: SymbolTable): Type = {
    expr match {
      case ident: Ident => symbolTable.lookUp(ident.name, ident.pos) match {
      case Right(t) => t
      case Left(errors) => throw new RuntimeException(errors.mkString(", "))
      }
      case IntLiter(value) => IntType
      case BoolLiter(value) => BoolType
      case CharLiter(value) => CharType
      case StrLiter(value) => StringType
      case ArrayElem(ident, args) => symbolTable.lookUp(ident.name, ident.pos) match {
        case Right(ArrayType(t)) => getInnerArrayType(t, args.length)
        case Right(_) => throw new RuntimeException("Not an array")
        case Left(errors) => throw new RuntimeException(errors.mkString(", "))
      }
      case PairLiter() => ErasedPairType
      case Bracket(expr) => ExprToType(expr, symbolTable) 
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



  /* 
    Call Check
      Checks that the function return type matches the expected type
      and that all arguments match the function declaration
  */
  private def checkCall(
    call: Call, 
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] = {
    val argTypes = call.args.value.map(arg=> ExprToType(arg, symbolTable))

    val funcTypeFromSymbolTable = symbolTable.lookUpFunc(call.ident.name, argTypes, call.ident.pos)
    // val funcType = symbolTable.lookUpFunc(call.ident.name, call.ident.pos)
    funcTypeFromSymbolTable.flatMap {
      // Check return type matching, argument size matching, and argument type matching
      case FuncType(t, fargs) => {
        val returnTypeErrs = isType(t, call.pos)
        val argsErrs = {
          // If number of arguments don't match, return an error
          if (call.args.value.length != fargs.length) {
            Left(List(OtherError("Function Argument", None, None, None, 
              Some("Function arguments do not match declaration"), call.args.pos)))
          }
          // If no arguments, return the function type
          else if (call.args.value.isEmpty) Right(t)
          // Check if the arguments match the function declaration
          else {
            call.args.value.zip(fargs).map { 
              case (expr, expected) => checkExpr(expr, canWeakenTo(expected), symbolTable)
            }.reduce(merge).map(_ => t)
          }
        }
        merge(returnTypeErrs, argsErrs)
      }
      // Handle unexpected types with a catch-all case
      case _ => Left(List(OtherError("Type Error", None, None, None,
                  Some("Unexpected type encountered during function lookup"), call.ident.pos)))
    }.addContext(s"function call to ${call.ident.name}")
  }

  /* 
    Pair Element Check
      Checks whether the lValue can be unpacked and if the type matches the expected type
  */
  private def checkPairElem(
    pairElem: PairElem,
    isType: (Type, (Int, Int)) => Either[List[WaccError], Type],
    symbolTable: SymbolTable
  ): Either[List[WaccError], Type] = {
    // unpack the pair
    val unpackedPair = pairElem match {
      case Fst(lValue) =>
        checkLValue(lValue, unpackPair(_, fst = true, _), symbolTable)
      case Snd(lValue) =>
        checkLValue(lValue, unpackPair(_, fst = false, _), symbolTable)
      }
    // checks if the unpacked pair matches the expected type
    unpackedPair.flatMap(t => isType(t, pairElem.pos))
  }

  // Helper function to unpack a pair
  private def unpackPair(t: Type, fst: Boolean, pos: (Int, Int)): Either[List[WaccError], Type] = t match {
    case PairType(t1, t2) => {
      (if (fst) t1 else t2) match {
        case p: Type => Right(p)
        case pair    => Right(ErasedPairType)
      }
    }
    case ErasedPairType => Right(AnyType)
    case AnyType        => Right(AnyType)
    case _              => Left(List(TypeError(None, Some(t.toString()), Some("Pair"), None, pos)))
  }
}