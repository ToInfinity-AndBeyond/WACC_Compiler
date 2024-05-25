package wacc.codegen.aarch64

import wacc.ir.threeAddrCode.ThreeAddrInstr
import wacc.ir.threeAddrCode._
import aarch64Instr._
import scala.collection.mutable.ListBuffer

// usedLocs: number of used registers or stack locations
// argLocs: number of argument registers
class RegisterManager(val usedLocs: Int, val argLocs: Int) {
  private final val SUCCESS_EXIT_CODE: Int = 0

  private final val BYTE_SIZE: Int = 4 // 4 bytes per word
  private final val ALIGNMENT: Int = 8 // 8 byte alignment
  private final val TWO_REG_SIZE: Int = 16 // 2 registers are 16 bytes
  private final val ARG_REG: List[Register] = List(X0(X), X1(X), X2(X), X3(X), X4(X), X5(X), X6(X), X7(X)) // 4 argument registers (R0 to R3)
  private final val GEN_REG: List[Register] = List(X19(X), X20(X), X21(X), X22(X), X23(X), X24(X), X25(X), X26(X), X27(X), X28(X)) // General-purpose registers (R4 to R8)
  private final val TEMP_REG: List[Register] = List(X8(X), X17(X)) // 2 temporary registers (R9, R10)
  
  private val argRegisters = ARG_REG.take(argLocs) // argument registers used
  private val argStack     = Math.max(argLocs - ARG_REG.size, 0) * BYTE_SIZE // stack space used for arguments
  private val genRegisters = GEN_REG.take(usedLocs) // general-purpose registers used
  private val genStack     = Math.max(usedLocs - GEN_REG.size, 0) * BYTE_SIZE // stack space used for general-purpose registers
  private val stackOffset = genStack // total stack space used
  private var stackPadding = 0 // used to determine if stack padding is needed when calling funcs
  private var offset = 0 // used to determine the offset from the frame pointer (FP)
  // Used to determine the type of the location and call the appropriate function to get the StackRegister.
  private def getStackRegFromLoc(tLoc: TLoc, size: RegSize): (StackRegister, List[Instr]) = {
    tLoc match {
      case tArgLoc: TArgLoc => getStackRegFromArgLoc(tArgLoc, size)
      case tRegLoc: TRegLoc => getStackRegFromGenLoc(tRegLoc, size)
      case TTempLoc1 => (X8(size), List())
      case TTempLoc2 => (X17(size), List())
    }
  }
  
  // Used to get the StackRegister from an argument location.
  private def getStackRegFromArgLoc(tArgLoc: TArgLoc, size: RegSize): (StackRegister, List[Instr]) = {
    // If the id of the argument location is less than the size of the argument registers,
    if (tArgLoc.id  < argRegisters.size) {
      // Return the corresponding argument register.
      (argRegisters(tArgLoc.id).setSize(size), List())
    } else {
      val ofst = -((argStack - (tArgLoc.id - argRegisters.size + 1) * BYTE_SIZE * 2) + offset)
      if (ofst > 256) {
        (StackOffsetReg(SP, X17(X)), List(Instr(Mov, List(X17(X), Imm(-ofst)))))
      } else {
        (Stack(SP, -ofst), List())
      }
      // Calculate the offset from the frame pointer (FP) and return a Stack object
    }
  }

  // Used to get the StackRegister from a general location.
  private def getStackRegFromGenLoc(tRegLoc: TRegLoc, size: RegSize): (StackRegister, List[Instr]) = {
    // If the id of the general location is less than the size of the general registers, 
    if (tRegLoc.id < genRegisters.size) {
      // Return the corresponding general register.
      (genRegisters(tRegLoc.id).setSize(size), List())
    } else {
      val ofst = (genStack - ((tRegLoc.id - (genRegisters.size)) * BYTE_SIZE * 2))
      if (ofst > 256) {
        (StackOffsetReg(SP, X17(X)), List(Instr(Mov, List(X17(X), Imm(-ofst)))))
      } else {
        (Stack(SP, -ofst), List())
      }
      // Calculate the offset from the frame pointer (FP) and return a Stack object. 
      
    }
  }

  def pushRegisters(registers: List[Register]): List[Instr] = {
    val numRegs: Int = registers.length
    var regs: ListBuffer[(Register, Register)] = ListBuffer()
    if (numRegs % 2 != 0) {
      regs = ListBuffer((registers :+ XZR).sliding(2, 2).collect { case List(a, b) => (a, b) }.toList: _*)
    } else {
      regs = ListBuffer((registers :+ XZR).sliding(2, 2).collect { case List(a, b) => (a, b) }.toList: _*)
    }
    val instrs: ListBuffer[Instr] = ListBuffer()
    if (regs.size > 0) {
      instrs += Instr(StpUpdate, List(regs(0)._1, regs(0)._2, Stack(SP, -(TWO_REG_SIZE * (regs.length)))))
      for ((regPair, i) <- regs.drop(1).zipWithIndex) {
        instrs += Instr(Stp, List(regPair._1, regPair._2, Stack(SP, (i + 1) * TWO_REG_SIZE)))
      }
    }
    instrs.toList
  }

  def popRegisters(registers: List[Register]): List[Instr] = {
    val numRegs: Int = registers.length
    var regs: ListBuffer[(Register, Register)] = ListBuffer()
    if (numRegs % 2 != 0) {
      regs = ListBuffer((registers :+ XZR).sliding(2, 2).collect { case List(a, b) => (a, b) }.toList: _*)
    } else {
      regs = ListBuffer((registers :+ XZR).sliding(2, 2).collect { case List(a, b) => (a, b) }.toList: _*)
    }
    var instrs: ListBuffer[Instr] = ListBuffer()
    val tailInstrs: ListBuffer[Instr] = ListBuffer()
    if (regs.size > 0) {
      for ((regPair, i) <- regs.drop(1).zipWithIndex) {
        tailInstrs += Instr(Ldp, List(regPair._1, regPair._2, Stack(SP, (i + 1) * TWO_REG_SIZE)))
      }
      val firstI = Instr(LdpUpdate, List(regs(0)._1, regs(0)._2, StackReg(SP), Imm((TWO_REG_SIZE * (regs.length)))))
      instrs = tailInstrs ++ ListBuffer(firstI)
    }
    instrs.toList
  }

  // Used to save the current state of the stack.
  def saveStack(): List[Instr] = {
    // If there are any argument registers, push them onto the stack.
    if (argRegisters.size > 0) pushRegisters(ARG_REG) :+ Instr(Mov, List(X16(X), SP))
    else List()
  }

  // Used to restore the state of the stack.
  def restoreStack(): List[Instr] = {
    // If there were any argument registers, pop them from the stack.
    if (argRegisters.size > 0) popRegisters(ARG_REG)
    else List()
  }

  // Ued to calculate the padding needed to align the stack.
  private def getPaddingNum(n: Int): Int = {
    // If the given number is divisible by the alignment, it returns 0.
    if (n % ALIGNMENT == 0) 0
    // Otherwise, it returns the difference between the alignment and the remainder of the number divided by the alignment.
    else ALIGNMENT - (n % ALIGNMENT)
  }

  private def getPaddingInstr(isSetup: Boolean): List[Instr] = {
    val padding = stackOffset + getPaddingNum(stackOffset + getPushRegisters.size * BYTE_SIZE)
    if (padding != 0) {
      val comment = List(Instr(Comment("SP padded to required alignment"), List()))
      if (isSetup) comment ++ List(Instr(Sub, List(SP, SP, Imm(padding))))
      else comment ++ List(Instr(Add, List(SP, SP, Imm(padding))))
    }
    else List()
  }
  def setupFunc(): List[Instr] = {
    offset = (2 + getPushRegisters.size) * BYTE_SIZE
    pushRegisters(List(FP, LR)) ++
      pushRegisters(getPushRegisters) ++
      List(Instr(Mov, List(FP, SP))) ++
      getPaddingInstr(isSetup = true)
  }

  // Handles the function exit by popping the registers and unpadding the stack. 
  // It also moves the return value to the R0 register.
  def exitFunc(retTLoc: TLoc): List[Instr] = {
    getPaddingInstr(isSetup = false) ++
      moveToRegister(X0(X), retTLoc) ++
      popRegisters(getPushRegisters)  ++
      popRegisters(List(FP, LR)) ++ List(Instr(Ret, List()))
  }

  // This function is specifically for the main function. It sets the exit code to `SUCCESS_EXIT_CODE = 0`.
  def exitMain(): List[Instr] = {
    getPaddingInstr(isSetup = false) ++
      List(Instr(Mov, List(X0(X), Imm(SUCCESS_EXIT_CODE)))) ++ 
      popRegisters(getPushRegisters) ++
      popRegisters(List(FP, LR)) ++ List(Instr(Ret, List()))
  }

  // Returns a list of registers that need to be pushed onto the stack, including general purpose registers and a temporary register.
  private def getPushRegisters: List[Register] = (genRegisters  ++ TEMP_REG).sortBy(_.id)

  // The tloc register is loaded into the i-th argument register for a n-arity function
  private def loadFunctionArguments(i: Int, n: Int, tloc: TLoc): List[Instr] = {
    val stackReg: StackRegister = {
      if (i < ARG_REG.size) ARG_REG(i)
      else Stack(SP, (n - i - 1) * BYTE_SIZE)
    }
    moveToRegister(X8(X), tloc) ++ moveFromRegister(stackReg, X8(X))
  }

  // Prepares the function call and returns the entry and exit instructions
  def callFunction(args: List[TLoc]): (List[Instr], List[Instr]) = {
    val pushInstrs: List[Instr] = { // Push the argument registers
      if (argRegisters.size > 0) pushRegisters(argRegisters)
      else List()
    }
    val moveSP = List(Instr(Mov, List(X16(X), SP))) // Move the stack pointer to R12
    val align = { // Align the stack
      if (args.size >= ARG_REG.size) {
        val offset = (args.size - ARG_REG.size) * BYTE_SIZE
        val padding = offset + getPaddingNum(offset)
        if (padding != 0) List(Instr(Sub, List(SP, SP, Imm(padding))))
        else List()
      }
      else List()
    }
    // Setup the arguments
    val setupArgs = args.zipWithIndex.flatMap { 
      case (arg, i) => loadFunctionArguments(i, args.size, arg)
    }
   // Align the stack back
    val alignBack = { 
      if (args.size >= ARG_REG.size) {
        val offset = (args.size - ARG_REG.size) * BYTE_SIZE
        val padding = offset + getPaddingNum(offset)
        if (padding != 0) List(Instr(Add, List(SP, SP, Imm(padding))))
        else List()
      }
      else List()
    }
    // Pop the argument registers
    val popInstrs: List[Instr] = {
      if (argRegisters.size > 0) popRegisters(argRegisters)
      else List()
    }
    // Return the entry and exit instructions
    val entry: List[Instr] = pushInstrs ++ moveSP ++ align ++ setupArgs
    val exit: List[Instr] = alignBack ++ popInstrs
    (entry, exit)
  }

  // assign immeadiate value to a stack register represented by tloc
  def assignImm(dest: TLoc, imm: TImm): List[Instr] = {
    val loadInstr = Instr(Mov, List(X8(W), Imm(imm.value)))
    List(loadInstr) ++ moveFromRegister(dest, X8(W))
  }

  // assign tloc from src to dest
  def assignTLocs(dest: TLoc, src: TLoc): List[Instr] = {
    val (destStackReg, destInstr) = getStackRegFromLoc(dest, X)
    val (srcStackReg, srcInstr) = getStackRegFromLoc(src, X)
    srcInstr ++ moveToRegister(X8(X), srcStackReg) ++ destInstr ++ moveFromRegister(destStackReg, X8(X))
  }

  // Moves the value from the tloc to the dest register
  def moveToRegister(dest: Register, tloc: TLoc): List[Instr] = moveToRegister(dest, getStackRegFromLoc(tloc, dest.getSize())._1, getStackRegFromLoc(tloc, dest.getSize())._2)
  private def moveToRegister(dest: Register, loc: StackRegister) = {
    loc match {
      // If loc is a register, move the value from loc to dest
      case reg: Register => List(Instr(Mov, List(dest, reg)))
      // If loc is a stack, load the value from loc to dest
      case stack: Stack  => List(Instr(Ldr, List(dest, stack)))
      case stackReg: StackReg => List(Instr(Ldr, List(dest, stackReg)))
      case stackOffsetReg: StackOffsetReg => List(Instr(Ldr, List(dest, stackOffsetReg)))
    }
  }
  private def moveToRegister(dest: Register, loc: StackRegister, instrs: List[Instr]): List[Instr] = {
    loc match {
      // If loc is a register, move the value from loc to dest
      case reg: Register => instrs ++ List(Instr(Mov, List(dest, reg)))
      // If loc is a stack, load the value from loc to dest
      case stack: Stack  => instrs ++ List(Instr(Ldr, List(dest, stack)))
      case stackReg: StackReg => instrs ++ List(Instr(Ldr, List(dest, stackReg)))
      case stackOffsetReg: StackOffsetReg => instrs ++ List(Instr(Ldr, List(dest, stackOffsetReg)))
    }
  }

  // Moves the value from the src register to the tloc
  def moveFromRegister(tloc: TLoc, src: Register): List[Instr] = moveFromRegister(getStackRegFromLoc(tloc, src.getSize())._1, src, getStackRegFromLoc(tloc, src.getSize())._2)
  private def moveFromRegister(loc: StackRegister, src: Register): List[Instr] = {
    loc match {
      // If loc is a register, move the value from src to loc
      case reg: Register => List(Instr(Mov, List(reg, src)))
      // If loc is a stack, store the value from src to loc
      case stack: Stack  => List(Instr(Str, List(src, stack)))
      case stackReg: StackReg => List(Instr(Str, List(src, stackReg)))
      case stackOffsetReg: StackOffsetReg => List(Instr(Str, List(src, stackOffsetReg)))
    }
  }
  private def moveFromRegister(loc: StackRegister, src: Register, instrs: List[Instr]): List[Instr] = {
    loc match {
      // If loc is a register, move the value from src to loc
      case reg: Register => instrs ++ List((Instr(Mov, List(reg, src))))
      // If loc is a stack, store the value from src to loc
      case stack: Stack  => instrs ++ List(Instr(Str, List(src, stack)))
      case stackReg: StackReg => instrs ++ List(Instr(Str, List(src, stackReg)))
      case stackOffsetReg: StackOffsetReg => instrs ++ List(Instr(Str, List(src, stackOffsetReg)))
    }
  }
}