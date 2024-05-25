package wacc.codegen.arm32

import wacc.ir.threeAddrCode.ThreeAddrInstr
import wacc.ir.threeAddrCode._
import arm32Instr._

// usedLocs: number of used registers or stack locations
// argLocs: number of argument registers
class RegisterManager(val usedLocs: Int, val argLocs: Int) {
  private final val SUCCESS_EXIT_CODE: Int = 0

  private final val BYTE_SIZE: Int = 4 // 4 bytes per word
  private final val ALIGNMENT: Int = 8 // 8 byte alignment
  private final val ARG_REG: List[Register] = List(R0, R1, R2, R3) // 4 argument registers (R0 to R3)
  private final val GEN_REG: List[Register] = List(R4, R5, R6, R7, R8) // General-purpose registers (R4 to R8)
  private final val TEMP_REG: List[Register] = List(R9, R10) // 2 temporary registers (R9, R10)
  private final val IMM_LOWER_BOUND = 0
  private final val IMM_UPPER_BOUND = 255
  
  private val argRegisters = ARG_REG.take(argLocs) // argument registers used
  private val argStack     = Math.max(argLocs - ARG_REG.size, 0) * BYTE_SIZE // stack space used for arguments
  private val genRegisters = GEN_REG.take(usedLocs) // general-purpose registers used
  private val genStack     = Math.max(usedLocs - GEN_REG.size, 0) * BYTE_SIZE // stack space used for general-purpose registers
  private val stackOffset = genStack // total stack space used
  private var stackPadding = 0 // used to determine if stack padding is needed when calling funcs
  private var offset = 0 // used to determine the offset from the frame pointer (FP)
  // Used to determine the type of the location and call the appropriate function to get the StackRegister.
  private def getStackRegFromLoc(tLoc: TLoc): StackRegister = {
    tLoc match {
      case tArgLoc: TArgLoc => getStackRegFromArgLoc(tArgLoc)
      case tRegLoc: TRegLoc => getStackRegFromGenLoc(tRegLoc)
      case TTempLoc1 => R9
      case TTempLoc2 => R10
    }
  }
  
  // Used to get the StackRegister from an argument location.
  private def getStackRegFromArgLoc(tArgLoc: TArgLoc): StackRegister = {
    // If the id of the argument location is less than the size of the argument registers,
    if (tArgLoc.id  < argRegisters.size) {
      // Return the corresponding argument register.
      argRegisters(tArgLoc.id)
    } else {
      // Calculate the offset from the frame pointer (FP) and return a Stack object
      Stack(FP, (argStack - (tArgLoc.id - argRegisters.size + 1) * BYTE_SIZE) + offset)
    }
  }

  // Used to get the StackRegister from a general location.
  private def getStackRegFromGenLoc(tRegLoc: TRegLoc): StackRegister = {
    // If the id of the general location is less than the size of the general registers, 
    if (tRegLoc.id < genRegisters.size) {
      // Return the corresponding general register.
      genRegisters(tRegLoc.id)
    } else {
      // Calculate the offset from the frame pointer (FP) and return a Stack object. 
      Stack(FP, -(genStack - ((tRegLoc.id - (genRegisters.size)) * BYTE_SIZE)))
    }
  }

  // Used to save the current state of the stack.
  def saveStack(): List[Instr] = {
    // If there are any argument registers, push them onto the stack.
    if (argRegisters.size > 0) List(Instr(Push, ARG_REG), Instr(Mov, List(R12, SP))) 
    else List()
  }

  // Used to restore the state of the stack.
  def restoreStack(): List[Instr] = {
    // If there were any argument registers, pop them from the stack.
    if (argRegisters.size > 0) List(Instr(Pop, ARG_REG)) 
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
    List(Instr(Push, List(FP, LR))) ++
      List(Instr(Push, getPushRegisters)) ++
      List(Instr(Mov, List(FP, SP))) ++
      getPaddingInstr(isSetup = true)
  }

  // Handles the function exit by popping the registers and unpadding the stack. 
  // It also moves the return value to the R0 register.
  def exitFunc(retTLoc: TLoc): List[Instr] = {
    getPaddingInstr(isSetup = false) ++
      moveToRegister(R0, retTLoc) ++
      List(Instr(Pop, getPushRegisters))  ++
      List(Instr(Pop, List(FP, PC)))
  }

  // This function is specifically for the main function. It sets the exit code to `SUCCESS_EXIT_CODE = 0`.
  def exitMain(): List[Instr] = {
    getPaddingInstr(isSetup = false) ++
      List(Instr(Mov, List(R0, Imm(SUCCESS_EXIT_CODE)))) ++ 
      List(Instr(Pop, getPushRegisters)) ++
      List(Instr(Pop, List(FP, PC)))
  }

  // Returns a list of registers that need to be pushed onto the stack, including general purpose registers and a temporary register.
  private def getPushRegisters: List[Register] = (genRegisters ++ TEMP_REG).sortBy(_.id)

  // The tloc register is loaded into the i-th argument register for a n-arity function
  private def loadFunctionArguments(i: Int, n: Int, tloc: TLoc): List[Instr] = {
    val stackReg: StackRegister = {
      if (i < ARG_REG.size) ARG_REG(i)
      else Stack(SP, (n - i - 1) * BYTE_SIZE)
    }
    moveToRegister(R9, tloc) ++ moveFromRegister(stackReg, R9)
  }

  // Prepares the function call and returns the entry and exit instructions
  def callFunction(args: List[TLoc]): (List[Instr], List[Instr]) = {
    val pushInstrs: List[Instr] = { // Push the argument registers
      if (argRegisters.size > 0) List(Instr(Push, argRegisters)) 
      else List()
    }
    val moveSP = List(Instr(Mov, List(R12, SP))) // Move the stack pointer to R12
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
      if (argRegisters.size > 0) List(Instr(Pop, argRegisters)) 
      else List()
    }
    // Return the entry and exit instructions
    val entry: List[Instr] = pushInstrs ++ moveSP ++ align ++ setupArgs
    val exit: List[Instr] = alignBack ++ popInstrs
    (entry, exit)
  }

  // assign immeadiate value to a stack register represented by tloc
  def assignImm(dest: TLoc, imm: TImm): List[Instr] = {
    val loadInstr = 
      if (IMM_LOWER_BOUND <= imm.value && imm.value <= IMM_UPPER_BOUND) {
        Instr(Mov, List(R9, Imm(imm.value)))
      } else {
        Instr(Ldr, List(R9, Imm(imm.value)))
      }
    List(loadInstr) ++ moveFromRegister(dest, R9)
  }

  // assign tloc from src to dest
  def assignTLocs(dest: TLoc, src: TLoc): List[Instr] = {
    val destStackReg = getStackRegFromLoc(dest)
    val srcStackReg = getStackRegFromLoc(src)
    moveToRegister(R9, srcStackReg) ++ moveFromRegister(destStackReg, R9)
  }

  // Moves the value from the tloc to the dest register
  def moveToRegister(dest: Register, tloc: TLoc): List[Instr] = moveToRegister(dest, getStackRegFromLoc(tloc))
  private def moveToRegister(dest: Register, loc: StackRegister) = {
    loc match {
      // If loc is a register, move the value from loc to dest
      case reg: Register => List(Instr(Mov, List(dest, reg)))
      // If loc is a stack, load the value from loc to dest
      case stack: Stack  => List(Instr(Ldr, List(dest, stack)))
    }
  }
  // Moves the value from the src register to the tloc
  def moveFromRegister(tloc: TLoc, src: Register): List[Instr] = moveFromRegister(getStackRegFromLoc(tloc), src)
  private def moveFromRegister(loc: StackRegister, src: Register): List[Instr] = {
    loc match {
      // If loc is a register, move the value from src to loc
      case reg: Register => List(Instr(Mov, List(reg, src)))
      // If loc is a stack, store the value from src to loc
      case stack: Stack  => List(Instr(Str, List(src, stack)))
    }
  }
}