package wacc.codegen.arm32

import scala.collection.immutable
import wacc.ir.IR
import wacc.ir.threeAddrCode.TStringLabel
import wacc.ir.ThreeAddrFunc

import arm32Instr._
import Dependencies.getFunction

import ThreeAddressToArm32.convertFunc
import wacc.codegen.FileHandler.appendToFile

object arm32StringBuilder {
  // Generates ARM32 assembly code from the intermediate representation (IR)
  def generate(ir: IR, destFile: String): Unit = {
    // output headers
    addHeaders(destFile, ir.strings)
    
    // collect all the required predefined functions
    val dependencies: Dependencies = new Dependencies()
    var isFirstFunc = true
    // For each function in the IR, convert it to ARM32 and add it to the assembly file.
    ir.funcs.foreach { threeAddrFunc =>
      // Add the function to the assembly file and get its dependencies
      val dependency = addFunc(destFile, threeAddrFunc, isFirstFunc)
      dependencies.addDependencies(dependency)
      isFirstFunc = false
    }
    // Output branch links
    addPredefinedFuncsToFile(destFile, dependencies.getDependencies())
  }

  // Adds headers to the assembly file.
  private def addHeaders(destFile: String, strings: List[TStringLabel]): Unit = {
    // Create a list of header strings from the list of TStringLabel objects.
    val headerStrings: List[String] = {
      strings.map(string => {
        val label = s".L.str${string.id}"
        val wordLen = string.value.length
        val escapedString = string.value.replace("\\", "\\\\").replace("\"", "\\\"")
        s"@ length of ${label}\n\t.word $wordLen\n${label}:\n\t.asciz \"$escapedString\""
      })
    }

    // start section
    // .data, strings, .align 4, .text, .global main
    val header: List[String] = 
      List(".data") ++
      headerStrings ++
      List(".align 4", ".text", ".global main")
    appendToFile(destFile, header.mkString("\n"))
  }

  // Converts a function from the IR to ARM32, adds it to the assembly file, and returns its dependencies.
  private def addFunc(destFile: String, threeAddrFunc: ThreeAddrFunc, isMain: Boolean): immutable.Set[Dependency] = {
    val (arm32Func, branch) = convertFunc(threeAddrFunc, isMain)
    val label = if (isMain) threeAddrFunc.name else renameFuncLabel(threeAddrFunc.name)
    val ltorg = if (arm32Func.ltorg) "\n\t.ltorg" else ""
    val content = {
      "\n" + label + ":\n" +
      arm32Func.instrs.map(instr => s"${instrToStr(instr)}").mkString("\n") + ltorg + "\n"
    }
    appendToFile(destFile, content)
    branch
  }

  // Adds predefined functions to the assembly file.
  private def addPredefinedFuncsToFile(destFile: String, branchLinks: immutable.Set[Dependency]): Unit = {
    branchLinks.foreach {
      branchLink => {
        val content = getFunction(branchLink)
        appendToFile(destFile, content + "\n")
      }
    }
  }

  // Converts an instruction to a string.
  private def instrToStr(instr: Instr): String = {
    val str = instr.opcode match {
      // Data Processing Instructions
      case Add => "add " + operandsToStr(instr.operands)
      case Adds => "adds " + operandsToStr(instr.operands)
      case Sub => "sub " + operandsToStr(instr.operands)
      case Subs => "subs " + operandsToStr(instr.operands)
      case Mul => "mul " + operandsToStr(instr.operands)
      case Muls => "muls " + operandsToStr(instr.operands)
      case Smull => "smull " + operandsToStr(instr.operands)
      case And => "and " + operandsToStr(instr.operands)
      case Cmp => "cmp " + operandsToStr(instr.operands)

      // Branch Instructions
      case B => "b " + operandsToStr(instr.operands)
      case Beq => "beq " + operandsToStr(instr.operands)
      case Bne => "bne " + operandsToStr(instr.operands)
      case Bl => "bl " + operandsToStr(instr.operands)
      case Blne => "blne " + operandsToStr(instr.operands)
      case Bleq => "bleq " + operandsToStr(instr.operands)
      case Blvs => "blvs " + operandsToStr(instr.operands)

      // Load and Store Instructions
      case Mov => "mov " + operandsToStr(instr.operands)
      case Ldr => "ldr " + ldrOperandsToStr(instr.operands)
      case Str => "str " + operandsToStr(instr.operands)
      case Ldrsb => "ldrsb " + operandsToStr(instr.operands)
      case Strb => "strb " + operandsToStr(instr.operands)

      // Conditional Instructions
      case Moveq => "moveq " + operandsToStr(instr.operands)
      case Movne => "movne " + operandsToStr(instr.operands)
      case Movge => "movge " + operandsToStr(instr.operands)
      case Movlt => "movlt " + operandsToStr(instr.operands)
      case Movgt => "movgt " + operandsToStr(instr.operands)
      case Movle => "movle " + operandsToStr(instr.operands)

      // Stack Operations
      case Push => "push {" + operandsToStr(instr.operands) + "}"
      case Pop => "pop {" + operandsToStr(instr.operands) + "}"

      // Other Instructions
      case Rsbs => "rsbs " + operandsToStr(instr.operands)
      case UserFuncLabel(name) => renameFuncLabel(name) + ":"
      case BranchLabel(id) => renameBranchLabel(id) + ":"

      case Lsl => "lsl " + operandsToStr(instr.operands)
      case Tst => "tst " + operandsToStr(instr.operands)

      case Comment(comment) => "@ " + comment
    }
    if (instr.opcode.isInstanceOf[Label]) str
    else "\t" + str
  }

  // These functions rename labels.
  private def renameStringLabel(id: Int): String = ".L.str" + id
  private def renameFuncLabel(name: String): String = "wacc_" + name
  private def renameBranchLabel(id: Int): String = ".L" + id

  // These functions convert operands to strings.
  private def ldrOperandsToStr(operands: List[Operand]): String = {
    val dest = operands(0)
    val src = operands(1)
    val prefix = src match {
      case _ : Imm => "="
      case _: StrLabel => "="
      case _ => ""      
    }
    s"${operandToStr(dest)}, ${prefix}${operandToStr(src)}"
  }
  private def operandsToStr(operands: List[Operand]): String = {
    operands.map(operandToStr).mkString(", ")
  }

  private def operandToStr(operand: Operand): String = {
    operand match {
      case Imm(value) => s"#$value"
      case reg: Register => registerToStr(reg)
      case Stack(reg, offset) => s"[${operandToStr(reg)}, #${offset}]"
      case HeapLocImm(reg, offset) => s"[${registerToStr(reg)}, ${operandToStr(offset)}]"
      case HeapLocReg(reg, offset) => s"[${registerToStr(reg)}, ${operandToStr(offset)}]"

      case BranchLabel(id) => s".L${id}"
      case StrLabel(id) => renameStringLabel(id)
      case UserFuncLabel(name) => renameFuncLabel(name)

      case Asr(value) => s"asr #$value"

      case ArrLoad1 => "_arrLoad1"
      case ArrLoad4 => "_arrLoad4"
      case ArrStore1 => "_arrStore1"
      case ArrStore4 => "_arrStore4"
      case ErrBadChar => "_errBadChar"
      case ErrDivZero => "_errDivZero"
      case ErrNull => "_errNull"
      case ErrOutOfBounds => "_errOutOfBounds"
      case ErrOutOfMemory => "_errOutOfMemory"
      case ErrOverflow => "_errOverflow"
      case Exit => "_exit"
      case Free => "_free"
      case FreePair => "_freepair"
      case Malloc => "_malloc"
      case PrintBoolean => "_printb"
      case PrintChar => "_printc"
      case PrintInteger => "_printi"
      case Println => "_println"
      case PrintPointer => "_printp"
      case PrintString => "_prints"
      case ReadChar => "_readc"
      case ReadInteger => "_readi"
      case DivMod => "__aeabi_idivmod"
    }
  }

  // Converts a register to a string.
  private def registerToStr(register: Register): String = {
    register match {
      case R0 => "r0"
      case R1 => "r1"
      case R2 => "r2"
      case R3 => "r3"
      case R4 => "r4"
      case R5 => "r5"
      case R6 => "r6"
      case R7 => "r7"
      case R8 => "r8"
      case R9 => "r9"
      case R10 => "r10"
      case FP => "fp"
      case IP => "ip"
      case SP => "sp"
      case LR => "lr"
      case PC => "pc"
      case R12 => "r12"
      case _ => "r0"
    }
  }
}