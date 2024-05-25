package wacc.codegen.arm32

import scala.collection.immutable
import wacc.ir.Consts
import wacc.ir.ThreeAddrFunc
import wacc.ir.threeAddrCode._
import arm32Instr._

// Convert a function from three-address code to ARM32 assembly code
object ThreeAddressToArm32 {
  private final val FOUR_BYTE_SIZE: Int = 32
  private final val MEMORY_SIZE: Int = 4
  private final val TRUE: Int = 1
  private final val FALSE: Int = 0
  private final val ASCII_VALID_RANGE_START = -128
  private final val NEGATION_BASE_VALUE = 0
  private final val DIVISOR_CHECK_VALUE = 0


  // Converts a function from three-address code to ARM32 assembly code.
  def convertFunc(func: ThreeAddrFunc, isMain: Boolean): (arm32Func, immutable.Set[Dependency]) = {
    // A RegisterManager is used to manage the registers and stack used in the function.
    val registerManager: RegisterManager = new RegisterManager(func.usedLocs, func.argNum)
    val entry = registerManager.setupFunc()
    // The body of the function and any dependencies it has.
    val (body, dependency) = {
      var bodyAcc: List[Instr] = List.empty
      var dependencyAcc: Set[Dependency] = Set.empty
      // Convert each instruction in the function to ARM32 assembly code.
      for (instr <- func.instrs) {
        val (instrBody, dependencies) = convertInstr(instr, registerManager)
        bodyAcc ++= instrBody
        dependencyAcc ++= dependencies
      }
      (bodyAcc, dependencyAcc)
    }
    // The exit point of the function.
    val exit = if (isMain) registerManager.exitMain() else List()
    // The complete list of instructions in the function.
    val instrs: List[Instr] = entry ++ body ++ exit
    // Return the converted function and its dependencies.
    (arm32Func(func.name, instrs, true), dependency)
  }

  // This function converts each instruction from three-address code to ARM32 assembly code.
  private def convertInstr(instr: ThreeAddrInstr, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = instr match {
    // Assignments
    case TAssignLoc(dest, src) => (registerManager.assignTLocs(dest, src), Set())
    case TAssignImm(dest, value) => (registerManager.assignImm(dest, value), Set())

    // IO
    case TPrintB(loc, newLine) => convertPrint(loc, newLine, PrintBoolean, registerManager)
    case TPrintC(loc, newLine) => convertPrint(loc, newLine, PrintChar, registerManager)
    case TPrintI(loc, newLine) => convertPrint(loc, newLine, PrintInteger, registerManager)
    case TPrintP(loc, newLine) => convertPrint(loc, newLine, PrintPointer, registerManager)
    case TPrintS(loc, newLine) => convertPrint(loc, newLine, PrintString, registerManager)
    case TReadI(ret) => convertRead(ret, ReadInteger, registerManager)
    case TReadC(ret) => convertRead(ret, ReadChar, registerManager)

    // Arithmetic
    case binOp: TBinOp => convertBinOp(binOp, registerManager)
    case unOp: TUnOp => convertUnOp(unOp, registerManager)
    
    // Control Flow
    case TSkip() => (List(), Set())
    case TCall(func, args, ret) => convertFuncCall(func, args, ret, registerManager)
    case TReturn(op) => (registerManager.exitFunc(op), Set())
    case TExit(op) => (registerManager.moveToRegister(R0, op) ++ List(Instr(Bl, List(Exit))), Set(Exit))

    // Heap
    case TMallocArray(dest, size) => mallocArray(dest, size, isChar = false, registerManager)
    case TMallocCharArray(dest, size) => mallocArray(dest, size, isChar = true, registerManager)
    case TMallocPtrArray(dest, size) => mallocArray(dest, size, isChar = false, registerManager)
    case TMallocPair(dest) => mallocPair(dest, registerManager)
    case TFreeA(loc) => freeArray(loc, registerManager)
    case TFreeP(loc) => freePair(loc, registerManager)

    // Branching
    case TJump(label) => (List(Instr(B, List(BranchLabel(label.id)))), Set())
    case TJumpCond(cond, label) => 
      (registerManager.moveToRegister(R9, cond) ++ 
        List(Instr(Cmp, List(R9, Imm(TRUE))), Instr(Beq, List(BranchLabel(label.id)))), Set())

    case TBranchLabel(id) => (List(Instr(BranchLabel(id), List())), Set())
    case TFuncLabel(name) => (List(Instr(UserFuncLabel(name), List())), Set())
  }

  private def convertPrint(tloc: TLoc, newLine: Boolean, printFunc: Dependency, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.saveStack() ++ 
                  registerManager.moveToRegister(R0, tloc) ++ 
                  List(Instr(Bl, List(printFunc))) ++ 
                  (if (newLine) List(Instr(Bl, List(Println))) else List()) ++
                  registerManager.restoreStack()
    val dependencies = if (newLine) Set(printFunc, Println) else Set(printFunc)
    (instr, dependencies)
  }

  private def convertRead(tloc: TLoc, readFunc: Dependency, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.saveStack() ++ 
                  registerManager.moveToRegister(R0, tloc) ++ 
                  List(Instr(Bl, List(readFunc))) ++ 
                  registerManager.moveFromRegister(tloc, R0) ++
                  registerManager.restoreStack()
    (instr, Set(readFunc))
  }

  // Converts a function call in three-address code to ARM32 instructions.
  private def convertFuncCall(func: String, args: List[TLoc], ret: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val (entry, exit) = registerManager.callFunction(args)
    val instr = List(Instr(Bl, List(UserFuncLabel(func)))) ++ registerManager.moveFromRegister(ret, R0)
    (entry ++ instr ++ exit, Set())
  }

  // Directs unary operations to the appropriate handling method based on the operation type.
  private def convertUnOp(unOp: TUnOp, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = unOp match {
    case TNEG(dest, src) => calcNegOp(dest, src, registerManager)
    case TORD(dest, src) => calcOrdOp(dest, src, registerManager)
    case TCHR(dest, src) => calcChrOp(dest, src, registerManager)
    case TNOT(dest, src) => calcNotOp(dest, src, registerManager)
    case TLEN(dest, src) => calcLen(dest, src, registerManager)
  }

  // This function calculates the length of a string.
  private def calcLen(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R9, src) ++
                  List(Instr(Ldr, List(R9, Stack(R9, -MEMORY_SIZE)))) ++
                  registerManager.moveFromRegister(dest, R9)
    (instr, Set())
  }

  // This function calculates the ASCII value of a character.
  private def calcOrdOp(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    (registerManager.assignTLocs(dest, src), Set())
  }

  // This function calculates the character corresponding to an ASCII value.
  private def calcChrOp(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R9, src) ++
                List(Instr(Ldr, List(R10, Imm(ASCII_VALID_RANGE_START))), Instr(Tst, List(R9, R10)), 
                     Instr(Movne, List(R1, R9)), Instr(Blne, List(ErrBadChar))) ++
                registerManager.moveFromRegister(dest, R9)
    (instr, Set(ErrBadChar))
  }

  // This function calculates the negation of a number.
  private def calcNegOp(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R9, src) ++
                  List(Instr(Rsbs, List(R9, R9, Imm(NEGATION_BASE_VALUE)))) ++
                  List(Instr(Blvs, List(ErrOverflow))) ++
                  registerManager.moveFromRegister(dest, R9)
    (instr, Set(ErrOverflow))
  }

  // This function calculates the logical NOT of a boolean.
  private def calcNotOp(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R9, src) ++
                  List(Instr(Cmp, List(R9, Imm(TRUE)))) ++
                  List(Instr(Movne, List(R9, Imm(TRUE)))) ++
                  List(Instr(Moveq, List(R9, Imm(FALSE)))) ++
                  registerManager.moveFromRegister(dest, R9)
    (instr, Set())
  }
  
  // Converts binary operations from three-address code to ARM32 instructions.
  private def convertBinOp(binOp: TBinOp, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = binOp match {
    // Handles different binary operations by pattern matching the operation type and delegating to specific methods.
    case TMUL(dest, lvalue, rvalue) => calcMultOp(dest, lvalue, rvalue, registerManager)
    case TADD(dest, lvalue, rvalue) => calcBinIntOp(dest, lvalue, rvalue, Adds, registerManager)
    case TSUB(dest, lvalue, rvalue) => calcBinIntOp(dest, lvalue, rvalue, Subs, registerManager)
    case TEQ(dest, lvalue, rvalue) => calcEqualityOp(dest, lvalue, rvalue, true, registerManager)
    case TNEQ(dest, lvalue, rvalue) => calcEqualityOp(dest, lvalue, rvalue, false, registerManager)
    case TDIV(dest, lvalue, rvalue) => calcDivMod(dest, lvalue, rvalue, registerManager, true)
    case TMOD(dest, lvalue, rvalue) => calcDivMod(dest, lvalue, rvalue, registerManager, false)
    case TLT(dest, lvalue, rvalue) => calcInequalityOp(dest, lvalue, rvalue, true, false, registerManager)
    case TLTE(dest, lvalue, rvalue) => calcInequalityOp(dest, lvalue, rvalue, true, true, registerManager)
    case TGT(dest, lvalue, rvalue) => calcInequalityOp(dest, lvalue, rvalue, false, false, registerManager)
    case TGTE(dest, lvalue, rvalue) => calcInequalityOp(dest, lvalue, rvalue, false, true, registerManager)

    case TStore(srcTLoc, memoryDest, offset) => convertStore(srcTLoc, memoryDest, offset, isChar = false, registerManager)
    case TCharStore(srcTLoc, memoryDest, offset) => convertStore(srcTLoc, memoryDest, offset, isChar = true, registerManager)
    case TPtrStore(srcTLoc, memoryDest, offset) => convertStore(srcTLoc, memoryDest, offset, isChar = false, registerManager)
    case TArrElemStore(srcTLoc, memoryDest, offset) => convertArrayElemStore(srcTLoc, memoryDest, offset, isChar = false, registerManager)
    case TCharArrElemStore(srcTLoc, memoryDest, offset) => convertArrayElemStore(srcTLoc, memoryDest, offset, isChar = true, registerManager)
    case TPairElemStore(srcTLoc, memoryDest, offset) => convertPairElemStore(srcTLoc, memoryDest, offset, registerManager)
    case TPtrArrElemStore(srcTLoc, memoryDest, offset) => convertArrayElemStore(srcTLoc, memoryDest, offset, isChar = false, registerManager)
    
    case TLoad(destTLoc, memorySrc, offset) => convertLoad(destTLoc, memorySrc, offset, registerManager)
    case TCharArrElemLoad(destTLoc, memorySrc, offset) => convertArrayElemLoad(destTLoc, memorySrc, offset, isChar = true, registerManager)
    case TArrElemLoad(destTLoc, memorySrc, offset) => convertArrayElemLoad(destTLoc, memorySrc, offset, isChar = false, registerManager)
    case TPtrArrElemLoad(destTLoc, memorySrc, offset) => convertArrayElemLoad(destTLoc, memorySrc, offset, isChar = false, registerManager)
    case TPairElemLoad(destTLoc, memorySrc, offset) => convertPairElemLoad(destTLoc, memorySrc, offset, registerManager)
    case TStrLoad(loc, str) => {
      (List(Instr(Ldr, List(R9, StrLabel(str.id)))) ++ registerManager.moveFromRegister(loc, R9), Set())
    }
  }

  // This function calculates the division or modulus of two numbers.
  private def calcDivMod(dest: TLoc, lvalue: TLoc, rvalue: TLoc, registerManager: RegisterManager, isDiv: Boolean): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R0, lvalue) ++
                  registerManager.moveToRegister(R1, rvalue) ++
                  List(Instr(Cmp, List(R1, Imm(DIVISOR_CHECK_VALUE))), Instr(Bleq, List(ErrDivZero)), Instr(Bl, List(DivMod))) ++
                  registerManager.moveFromRegister(dest, {if (isDiv) R0 else R1})
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set(ErrDivZero))
  }

  // This function calculates a binary integer operation.
  private def calcBinIntOp(dest: TLoc, lvalue: TLoc, rvalue: TLoc, op: OpCode, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R9, lvalue) ++
                  registerManager.moveToRegister(R10, rvalue) ++
                  List(Instr(op, List(R9, R9, R10))) ++ 
                  List(Instr(Blvs, List(ErrOverflow))) ++
                  registerManager.moveFromRegister(dest, R9)
    (instr, Set(ErrOverflow))
  }

  // This function calculates the multiplication of two numbers.
  private def calcMultOp(dest: TLoc, lvalue: TLoc, rvalue: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R9, lvalue) ++
                  registerManager.moveToRegister(R10, rvalue) ++
                  List(Instr(Smull, List(R9, R10, R9, R10)),
                       Instr(Cmp, List(R10, R9, Asr(FOUR_BYTE_SIZE-1))), Instr(Blne, List(ErrOverflow))) ++
                  registerManager.moveFromRegister(dest, R9)
    (instr, Set(ErrOverflow))
  }

  // isLessThan and isEqual are used to determine the condition for inequality result
  private def calcInequalityOp(dest: TLoc, lvalue: TLoc, rvalue: TLoc, isLessThan: Boolean, isEqual: Boolean, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val movConds: List[Instr] = {
      if (isLessThan && isEqual) List(Instr(Movle, List(R9, Imm(TRUE))), Instr(Movgt, List(R9, Imm(FALSE))))
      else if (isLessThan && !isEqual) List(Instr(Movlt, List(R9, Imm(TRUE))), Instr(Movge, List(R9, Imm(FALSE))))
      else if (!isLessThan && isEqual) List(Instr(Movge, List(R9, Imm(TRUE))), Instr(Movlt, List(R9, Imm(FALSE))))
      else List(Instr(Movgt, List(R9, Imm(TRUE))), Instr(Movle, List(R9, Imm(FALSE))))
    }
    val instr = registerManager.moveToRegister(R9, lvalue) ++
                  registerManager.moveToRegister(R10, rvalue) ++
                  List(Instr(Cmp, List(R9, R10))) ++
                  movConds ++ 
                  registerManager.moveFromRegister(dest, R9)
    (instr, Set())
  }

  // Determine the condition for equality result
  private def calcEqualityOp(dest: TLoc, lvalue: TLoc, rvalue: TLoc, isEqual: Boolean, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val movConds: List[Instr] = {
      if (isEqual) List(Instr(Moveq, List(R9, Imm(TRUE))), Instr(Movne, List(R9, Imm(FALSE))))
      else List(Instr(Movne, List(R9, Imm(TRUE))), Instr(Moveq, List(R9, Imm(FALSE))))
    }
    val instr = registerManager.moveToRegister(R9, lvalue) ++
                  registerManager.moveToRegister(R10, rvalue) ++
                  List(Instr(Cmp, List(R9, R10))) ++
                  movConds ++ 
                  registerManager.moveFromRegister(dest, R9)
    (instr, Set())
  }

  // Allocates memory for a pair, reserving space for two pointers. The size is adjusted for the pair's length storage.
  private def mallocPair(dest: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val allocSize: Int = 2 * MEMORY_SIZE
    val instrs: List[Instr] = List(Instr(Mov, List(R0, Imm(allocSize))), Instr(Bl, List(Malloc))) ++ 
      registerManager.moveFromRegister(dest, R0)
    (registerManager.saveStack() ++ instrs ++ registerManager.restoreStack(), Set(Malloc))
  }
  
  // Allocates memory for an array and initializes it. The size is adjusted for the array's length storage.
  private def mallocArray(dest: TLoc, size: Int, isChar: Boolean, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val allocSize: Int = {
      if (isChar) (size + 1)
      else (size + 1) * MEMORY_SIZE
    }
    val instrs = List(Instr(Mov, List(R0, Imm(allocSize))), Instr(Bl, List(Malloc)),
                      Instr(Add, List(R0, R0, Imm(MEMORY_SIZE))), Instr(Mov, List(R9, Imm(size))), 
                      Instr(Str, List(R9, Stack(R0, -MEMORY_SIZE)))) ++
                    registerManager.moveFromRegister(dest, R0)
    (registerManager.saveStack() ++ instrs ++ registerManager.restoreStack(), Set(Malloc))
  }

  // frees a malloced pair in memory, stored as a pointer in loc
  private def freePair(loc: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.saveStack() ++ 
                  registerManager.moveToRegister(R0, loc) ++ 
                  List(Instr(Bl, List(FreePair))) ++ 
                  registerManager.restoreStack()
    (instr, Set(FreePair))
  }

  // frees a malloced array in memory, stored as a pointer in loc
  private def freeArray(loc: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.saveStack() ++
                  registerManager.moveToRegister(R9, loc) ++ 
                  List(Instr(Sub, List(R9, R9, Imm(MEMORY_SIZE))), 
                      Instr(Mov, List(R0, R9)), Instr(Bl, List(Free))) ++
                  registerManager.restoreStack()
    (instr, Set(Free))
  }

  // Converts a load operation from three-address code to ARM32 instructions, handling the loading of data from memory into a register.
  private def convertLoad(locDest: TLoc, memorySrc: TLoc, offset: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R10, offset) ++
                  List(Instr(Lsl, List(R10, R10, Imm(2)))) ++ // multiplies R10 by 4 to get the offset in bytes
                  registerManager.moveToRegister(R3, memorySrc) ++
                  List(Instr(Ldr, List(R3, HeapLocReg(R3, R10)))) ++
                  registerManager.moveFromRegister(locDest, R3)
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set())
  }

  // Converts a store operation, saving a value from a register to a memory location, with special handling for character storage.
  private def convertStore(srcTLoc: TLoc, memoryDest: TLoc, offset: TLoc, isChar: Boolean, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R10, offset) ++
                  (if (isChar) List() else List(Instr(Lsl, List(R10, R10, Imm(2))))) ++ // multiplies R10 by 4 to get the offset in bytes
                  registerManager.moveToRegister(R3, memoryDest) ++
                  registerManager.moveToRegister(R9, srcTLoc) ++
                  List(Instr(Str, List(R9, HeapLocReg(R3, R10))))
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set())
  }

  // Handles storing a value to a pair element in memory. It includes null pointer checks to ensure safety. 
  // The value is stored at the specified offset from the pair's base address.
  private def convertPairElemStore(srcTLoc: TLoc, memoryDest: TLoc, offset: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R3, memoryDest) ++
                  List(Instr(Cmp, List(R3, Imm(Consts.NULL))), Instr(Bleq, List(ErrNull))) ++
                  registerManager.moveToRegister(R10, offset) ++
                  List(Instr(Lsl, List(R10, R10, Imm(2)))) ++ // multiplies R10 by 4 to get the offset in bytes
                  registerManager.moveToRegister(R9, srcTLoc) ++
                  List(Instr(Str, List(R9, HeapLocReg(R3, R10)))) 
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set(ErrNull))
  }

  // Handles loading a value from a pair element in memory. Includes null pointer checks for safety.
  private def convertPairElemLoad(destTLoc: TLoc, memoryDest: TLoc, offset: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(R9, memoryDest) ++
                  List(Instr(Cmp, List(R9, Imm(Consts.NULL))), Instr(Bleq, List(ErrNull))) ++
                  registerManager.moveToRegister(R10, offset) ++
                  List(Instr(Lsl, List(R10, R10, Imm(2)))) ++ // multiplies R10 by 4 to get the offset in bytes
                  List(Instr(Ldr, List(R9, HeapLocReg(R9, R10)))) ++
                  registerManager.moveFromRegister(destTLoc, R9)
    (instr, Set(ErrNull))
  }

  // Handles storing a value in an array element, including bounds checking.
  private def convertArrayElemStore(srcTLoc: TLoc, memorySrc: TLoc, offset: TLoc, isChar: Boolean, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val bl: Instr = if (isChar) Instr(Bl, List(ArrStore1)) else Instr(Bl, List(ArrStore4))
    val dep: Set[Dependency] = if (isChar) Set(ArrStore1) else Set(ArrStore4)
    val instr = registerManager.moveToRegister(R10, offset) ++
                  registerManager.moveToRegister(R3, memorySrc) ++
                  registerManager.moveToRegister(R9, srcTLoc) ++
                  List(bl)
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), dep)
  }

   // Handles loading a value from an array element, including bounds checking.
  private def convertArrayElemLoad(locDest: TLoc, memorySrc: TLoc, offset: TLoc, isChar: Boolean, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val bl: Instr = if (isChar) Instr(Bl, List(ArrLoad1)) else Instr(Bl, List(ArrLoad4))
    val dep: Set[Dependency] = if (isChar) Set(ArrLoad1) else Set(ArrLoad4)
    val instr = registerManager.moveToRegister(R10, offset) ++
                  List(Instr(Push, List(R3))) ++ 
                  registerManager.moveToRegister(R3, memorySrc) ++
                  List(bl) ++
                  registerManager.moveFromRegister(locDest, R3) ++
                  List(Instr(Pop, List(R3)))
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), dep)
  }

}
