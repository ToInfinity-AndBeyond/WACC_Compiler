package wacc.codegen.aarch64

import scala.collection.immutable
import wacc.ir.Consts
import wacc.ir.ThreeAddrFunc
import wacc.ir.threeAddrCode._
import aarch64Instr._
import wacc.codegen.X86_64
import scala.collection.mutable.ListBuffer

// Convert a function from three-address code to ARM32 assembly code
object ThreeAddressToAarch64 {
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
    (arm32Func(func.name, instrs), dependency)
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
    case TExit(op) => (registerManager.moveToRegister(X0(X), op) ++ List(Instr(Bl, List(Exit))), Set())

    // Heap
    case TMallocArray(dest, size) => mallocArray(dest, size, Generic, registerManager)
    case TMallocCharArray(dest, size) => mallocArray(dest, size, Char, registerManager)
    case TMallocPtrArray(dest, size) => mallocArray(dest, size, Pointer, registerManager)
    case TMallocPair(dest) => mallocPair(dest, registerManager)
    case TFreeA(loc) => freeArray(loc, registerManager)
    case TFreeP(loc) => freePair(loc, registerManager)

    // Branching
    case TJump(label) => (List(Instr(B, List(BranchLabel(label.id)))), Set())
    case TJumpCond(cond, label) => 
      (registerManager.moveToRegister(X8(X), cond) ++ 
        List(Instr(Cmp, List(X8(X), Imm(TRUE))), Instr(Beq, List(BranchLabel(label.id)))), Set())

    case TBranchLabel(id) => (List(Instr(BranchLabel(id), List())), Set())
    case TFuncLabel(name) => (List(Instr(UserFuncLabel(name), List())), Set())
  }

  private def convertPrint(tloc: TLoc, newLine: Boolean, printFunc: Dependency, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.saveStack() ++ 
                  registerManager.moveToRegister(X0(X), tloc) ++ 
                  List(Instr(Bl, List(printFunc))) ++ 
                  (if (newLine) List(Instr(Bl, List(Println))) else List()) ++
                  registerManager.restoreStack()
    val dependencies = if (newLine) Set(printFunc, Println) else Set(printFunc)
    (instr, dependencies)
  }

  private def convertRead(tloc: TLoc, readFunc: Dependency, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.saveStack() ++ 
                  registerManager.moveToRegister(X0(W), tloc) ++ 
                  List(Instr(Bl, List(readFunc))) ++ 
                  registerManager.moveFromRegister(tloc, X0(X)) ++
                  registerManager.restoreStack()
    (instr, Set(readFunc))
  }

  // Converts a function call in three-address code to ARM32 instructions.
  private def convertFuncCall(func: String, args: List[TLoc], ret: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val (entry, exit) = registerManager.callFunction(args)
    val instr = List(Instr(Bl, List(UserFuncLabel(func)))) ++ registerManager.moveFromRegister(ret, X0(X))
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
    val instr = registerManager.moveToRegister(X8(X), src) ++
                  List(Instr(Ldr, List(X8(X), Stack(X8(X), -MEMORY_SIZE)))) ++
                  registerManager.moveFromRegister(dest, X8(X))
    (instr, Set())
  }

  // This function calculates the ASCII value of a character.
  private def calcOrdOp(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    (registerManager.assignTLocs(dest, src), Set())
  }

  // This function calculates the character corresponding to an ASCII value.
  private def calcChrOp(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X8(X), src) ++
                List(Instr(Tst, List(X8(X), HexImm(Integer.toHexString(ASCII_VALID_RANGE_START)))), 
                     Instr(Cselne, List(X1(X), X8(X), X1(X))), Instr(Bne, List(ErrBadChar))) ++
                registerManager.moveFromRegister(dest, X8(X))
    (instr, Set(ErrBadChar))
  }

  // This function calculates the negation of a number.
  private def calcNegOp(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X17(X), src) ++
                  List(Instr(Mov, List(X8(X), Imm(NEGATION_BASE_VALUE)))) ++
                  List(Instr(Subs, List(X8(X), X8(X), X17(X)))) ++
                  List(Instr(Bvs, List(ErrOverflow))) ++
                  registerManager.moveFromRegister(dest, X8(X))
    (instr, Set(ErrOverflow))
  }

  // This function calculates the logical NOT of a boolean.
  private def calcNotOp(dest: TLoc, src: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X8(X), src) ++
                  List(Instr(Cmp, List(X8(X), Imm(TRUE)))) ++
                  List(Instr(Csetne, List(X8(X)))) ++
                  registerManager.moveFromRegister(dest, X8(X))
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
    case TDIV(dest, lvalue, rvalue) => calcDiv(dest, lvalue, rvalue, registerManager)
    case TMOD(dest, lvalue, rvalue) => calcMod(dest, lvalue, rvalue, registerManager)
    case TLT(dest, lvalue, rvalue) => calcInequalityOp(dest, lvalue, rvalue, true, false, registerManager)
    case TLTE(dest, lvalue, rvalue) => calcInequalityOp(dest, lvalue, rvalue, true, true, registerManager)
    case TGT(dest, lvalue, rvalue) => calcInequalityOp(dest, lvalue, rvalue, false, false, registerManager)
    case TGTE(dest, lvalue, rvalue) => calcInequalityOp(dest, lvalue, rvalue, false, true, registerManager)

    case TStore(srcTLoc, memoryDest, offset) => convertStore(srcTLoc, memoryDest, offset, Generic, registerManager)
    case TCharStore(srcTLoc, memoryDest, offset) => convertStore(srcTLoc, memoryDest, offset, Char, registerManager)
    case TPtrStore(srcTLoc, memoryDest, offset) => convertStore(srcTLoc, memoryDest, offset, Pointer, registerManager)
    case TArrElemStore(srcTLoc, memoryDest, offset) => convertArrayElemStore(srcTLoc, memoryDest, offset, Generic, registerManager)
    case TCharArrElemStore(srcTLoc, memoryDest, offset) => convertArrayElemStore(srcTLoc, memoryDest, offset, Char, registerManager)
    case TPtrArrElemStore(srcTLoc, memoryDest, offset) => convertArrayElemStore(srcTLoc, memoryDest, offset, Pointer, registerManager)
    case TPairElemStore(srcTLoc, memoryDest, offset) => convertPairElemStore(srcTLoc, memoryDest, offset, registerManager)
    
    case TLoad(destTLoc, memorySrc, offset) => convertLoad(destTLoc, memorySrc, offset, registerManager)
    case TCharArrElemLoad(destTLoc, memorySrc, offset) => convertArrayElemLoad(destTLoc, memorySrc, offset, Char, registerManager)
    case TPtrArrElemLoad(destTLoc, memorySrc, offset) => convertArrayElemLoad(destTLoc, memorySrc, offset, Pointer, registerManager)
    case TArrElemLoad(destTLoc, memorySrc, offset) => convertArrayElemLoad(destTLoc, memorySrc, offset, Generic, registerManager)
    case TPairElemLoad(destTLoc, memorySrc, offset) => convertPairElemLoad(destTLoc, memorySrc, offset, registerManager)
    case TStrLoad(loc, str) => {
      (List(Instr(Adrp, List(X8(X), StrLabel(str.id))), Instr(Add, List(X8(X), X8(X), Reloc(StrLabel(str.id))))) ++ registerManager.moveFromRegister(loc, X8(X)), Set())
    }
  }

  // This function calculates the division or modulus of two numbers.
  private def calcDiv(dest: TLoc, lvalue: TLoc, rvalue: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X9(X), lvalue) ++
                  registerManager.moveToRegister(X10(X), rvalue) ++
                  List(Instr(Cbz, List(X10(X), ErrDivZero)), Instr(Sdiv, List(X8(W), X9(W), X10(W)))) ++
                  registerManager.moveFromRegister(dest, X8(W))
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set(ErrDivZero))
  }

  private def calcMod(dest: TLoc, lvalue: TLoc, rvalue: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X9(X), lvalue) ++
                  registerManager.moveToRegister(X10(X), rvalue) ++
                  List(Instr(Cbz, List(X10(X), ErrDivZero)), Instr(Sdiv, List(X17(W), X9(W), X10(W)))) ++
                  List(Instr(Msub, List(X8(W), X17(W), X10(W), X9(W)))) ++
                  registerManager.moveFromRegister(dest, X8(W))
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set(ErrDivZero))
  }

  // This function calculates a binary integer operation.
  private def calcBinIntOp(dest: TLoc, lvalue: TLoc, rvalue: TLoc, op: OpCode, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X8(X), lvalue) ++
                  registerManager.moveToRegister(X17(X), rvalue) ++
                  List(Instr(op, List(X8(X), X8(X), X17(X)))) ++ 
                  List(Instr(Bvs, List(ErrOverflow))) ++
                  registerManager.moveFromRegister(dest, X8(X))
    (instr, Set(ErrOverflow))
  }

  // This function calculates the multiplication of two numbers.
  private def calcMultOp(dest: TLoc, lvalue: TLoc, rvalue: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X9(W), lvalue) ++
                  registerManager.moveToRegister(X10(W), rvalue) ++
                  List(Instr(Smull, List(X8(X), X9(W), X10(W))),
                      Instr(Sbfx, List(X17(X), X8(X), Imm(FOUR_BYTE_SIZE - 1), Imm(1))),
                      Instr(Cmp, List(X17(X), X8(X), Asr(FOUR_BYTE_SIZE))), Instr(Bne, List(ErrOverflow))) ++
                  registerManager.moveFromRegister(dest, X8(X))
    (instr, Set(ErrOverflow))
  }

  // isLessThan and isEqual are used to determine the condition for inequality result
  private def calcInequalityOp(dest: TLoc, lvalue: TLoc, rvalue: TLoc, isLessThan: Boolean, isEqual: Boolean, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val movConds: List[Instr] = {
      if (isLessThan && isEqual) List(Instr(Csetle, List(X8(X))))
      else if (isLessThan && !isEqual) List(Instr(Csetlt, List(X8(X))))
      else if (!isLessThan && isEqual) List(Instr(Csetge, List(X8(X))))
      else List(Instr(Csetgt, List(X8(X))))
    }
    val instr = registerManager.moveToRegister(X8(X), lvalue) ++
                  registerManager.moveToRegister(X17(X), rvalue) ++
                  List(Instr(Cmp, List(X8(X), X17(X)))) ++
                  movConds ++ 
                  registerManager.moveFromRegister(dest, X8(X))
    (instr, Set())
  }

  // Determine the condition for equality result
  private def calcEqualityOp(dest: TLoc, lvalue: TLoc, rvalue: TLoc, isEqual: Boolean, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val movConds: List[Instr] = {
      if (isEqual) List(Instr(Cseteq, List(X8(X))))
      else List(Instr(Csetne, List(X8(X))))
    }
    val instr = registerManager.moveToRegister(X8(X), lvalue) ++
                  registerManager.moveToRegister(X17(X), rvalue) ++
                  List(Instr(Cmp, List(X8(X), X17(X)))) ++
                  movConds ++ 
                  registerManager.moveFromRegister(dest, X8(X))
    (instr, Set())
  }

  // Allocates memory for a pair, reserving space for two pointers. The size is adjusted for the pair's length storage.
  private def mallocPair(dest: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val allocSize: Int = 4 * MEMORY_SIZE 
    val instrs: List[Instr] = List(Instr(Mov, List(X0(W), Imm(allocSize))), Instr(Bl, List(Malloc))) ++ 
      registerManager.moveFromRegister(dest, X0(X))
    (registerManager.saveStack() ++ instrs ++ registerManager.restoreStack(), Set(Malloc))
  }
  
  // Allocates memory for an array and initializes it. The size is adjusted for the array's length storage.
  private def mallocArray(dest: TLoc, size: Int, valType: ValType, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val allocSize: Int = valType match {
      case Char => (size + 1)
      case Pointer => (size + 1) * MEMORY_SIZE * 4
      case Generic => (size + 1) * MEMORY_SIZE * 2
    }
    val instrs = List(Instr(Mov, List(X0(W), Imm(allocSize))), Instr(Bl, List(Malloc)), Instr(Mov, List(X16(X), X0(X))), 
                      Instr(Add, List(X16(X), X16(X), Imm(MEMORY_SIZE))), Instr(Mov, List(X8(W), Imm(size))), 
                      Instr(Stur, List(X8(W), Stack(X16(X), -MEMORY_SIZE)))) ++
                    registerManager.moveFromRegister(dest, X16(X))
    (registerManager.saveStack() ++ instrs ++ registerManager.restoreStack(), Set(Malloc))
  }

  // frees a malloced pair in memory, stored as a pointer in loc
  private def freePair(loc: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.saveStack() ++ 
                  registerManager.moveToRegister(X0(X), loc) ++ 
                  List(Instr(Bl, List(FreePair))) ++ 
                  registerManager.restoreStack()
    (instr, Set(FreePair))
  }

  // frees a malloced array in memory, stored as a pointer in loc
  private def freeArray(loc: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.saveStack() ++
                  registerManager.moveToRegister(X8(X), loc) ++ 
                  List(Instr(Sub, List(X8(X), X8(X), Imm(MEMORY_SIZE))), 
                      Instr(Mov, List(X0(X), X8(X))), Instr(Bl, List(Free))) ++
                  registerManager.restoreStack()
    (instr, Set())
  }

  // Converts a load operation from three-address code to ARM32 instructions, handling the loading of data from memory into a register.
  private def convertLoad(locDest: TLoc, memorySrc: TLoc, offset: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X17(X), offset) ++
                  List(Instr(Lsl, List(X17(X), X17(X), Imm(2)))) ++ // multiplies X17 by 4 to get the offset in bytes
                  registerManager.moveToRegister(X11(X), memorySrc) ++
                  List(Instr(Ldr, List(X11(X), HeapLocReg(X11(X), X17(X))))) ++
                  registerManager.moveFromRegister(locDest, X11(X))
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set())
  }

  // Converts a store operation, saving a value from a register to a memory location, with special handling for character storage.
  private def convertStore(srcTLoc: TLoc, memoryDest: TLoc, offset: TLoc, valType: ValType, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    var extraInstr: ListBuffer[Instr] = ListBuffer(Instr(Lsl, List(X17(W), X17(W), Imm(2))))
    var regSize: RegSize = W
    valType match {
      case Char => {
        extraInstr = ListBuffer()
        regSize = W
      }
      case Pointer => {
        extraInstr = ListBuffer(Instr(Lsl, List(X17(W), X17(W), Imm(3))))
        regSize = X
      }
      case Generic => 
    }
    val instr = registerManager.moveToRegister(X17(W), offset) ++
                  extraInstr ++ // multiplies X17 by 4 to get the offset in bytes
                  registerManager.moveToRegister(X11(X), memoryDest) ++
                  registerManager.moveToRegister(X8(regSize), srcTLoc) ++
                  List(Instr(Str, List(X8(regSize), HeapLocReg(X11(X), X17(X)))))
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set())
  }

  // Handles storing a value to a pair element in memory. It includes null pointer checks to ensure safety. 
  // The value is stored at the specified offset from the pair's base address.
  private def convertPairElemStore(srcTLoc: TLoc, memoryDest: TLoc, offset: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X11(X), memoryDest) ++
                  List(Instr(Cmp, List(X11(X), Imm(Consts.NULL))), Instr(Beq, List(ErrNull))) ++
                  registerManager.moveToRegister(X17(X), offset) ++
                  List(Instr(Lsl, List(X17(X), X17(X), Imm(2)))) ++ // multiplies X17 by 4 to get the offset in bytes
                  registerManager.moveToRegister(X8(X), srcTLoc) ++
                  List(Instr(Str, List(X8(X), HeapLocReg(X11(X), X17(X))))) 
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), Set(ErrNull))
  }

  // Handles loading a value from a pair element in memory. Includes null pointer checks for safety.
  private def convertPairElemLoad(destTLoc: TLoc, memoryDest: TLoc, offset: TLoc, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val instr = registerManager.moveToRegister(X8(X), memoryDest) ++
                  List(Instr(Cmp, List(X8(X), Imm(Consts.NULL))), Instr(Beq, List(ErrNull))) ++
                  registerManager.moveToRegister(X17(X), offset) ++
                  List(Instr(Lsl, List(X17(X), X17(X), Imm(2)))) ++ // multiplies X17 by 4 to get the offset in bytes
                  List(Instr(Ldr, List(X8(X), HeapLocReg(X8(X), X17(X))))) ++
                  registerManager.moveFromRegister(destTLoc, X8(X))
    (instr, Set(ErrNull))
  }

  // Handles storing a value in an array element, including bounds checking.
  private def convertArrayElemStore(srcTLoc: TLoc, memorySrc: TLoc, offset: TLoc, valType: ValType, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    var bl: Instr = Instr(Bl, List(ArrStore4))
    var dep: Set[Dependency] = Set(ArrStore4)
    valType match {
      case Char => {
        bl = Instr(Bl, List(ArrStore1))
        dep = Set(ArrStore1)
      }
      case Pointer => {
        bl = Instr(Bl, List(ArrStore8))
        dep = Set(ArrStore8)
      }
      case Generic =>
    }
    val instr = registerManager.moveToRegister(X17(W), offset) ++
                  registerManager.moveToRegister(X7(X), memorySrc) ++
                  registerManager.moveToRegister(X8(X), srcTLoc) ++
                  List(bl)
    (registerManager.saveStack() ++ instr ++ registerManager.restoreStack(), dep)
  }

   // Handles loading a value from an array element, including bounds checking.
  private def convertArrayElemLoad(locDest: TLoc, memorySrc: TLoc, offset: TLoc, valType: ValType, registerManager: RegisterManager): (List[Instr], Set[Dependency]) = {
    val (bl, dep): (Instr, Set[Dependency]) = valType match {
      case Char => (Instr(Bl, List(ArrLoad1)), Set(ArrLoad1))
      case Pointer => (Instr(Bl, List(ArrLoad8)), Set(ArrLoad8))
      case Generic => (Instr(Bl, List(ArrLoad4)), Set(ArrLoad4))
    }
    val instr = registerManager.moveToRegister(X17(W), offset) ++
                  registerManager.pushRegisters(List(X7(X))) ++ 
                  registerManager.moveToRegister(X7(X), memorySrc) ++
                  List(bl) ++
                  registerManager.moveFromRegister(locDest, X7(X)) ++
                  registerManager.popRegisters(List(X7(X)))
    (instr, dep)
  }

}