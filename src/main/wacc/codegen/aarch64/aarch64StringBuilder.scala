package wacc.codegen.aarch64

import scala.collection.immutable
import wacc.ir.IR
import wacc.ir.threeAddrCode.TStringLabel
import wacc.ir.ThreeAddrFunc

import aarch64Instr._
import Dependencies.getFunction

import ThreeAddressToAarch64.convertFunc
import wacc.codegen.FileHandler.appendToFile
import wacc.codegen.FileHandler

object aarch64StringBuilder {
  // Generates ARM32 assembly code from the intermediate representation (IR)
  def generate(ir: IR, destFile: String): Unit = {
    FileHandler.initializeFile(destFile)
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
        s"// length of ${label}\n\t.word $wordLen\n${label}:\n\t.asciz \"$escapedString\""
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
    val content = {
      "\n" + label + ":\n" +
      arm32Func.instrs.map(instr => s"${instrToStr(instr)}").mkString("\n") + "\n"
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
      case Msub => "msub " + operandsToStr(instr.operands)
      case Mul => "mul " + operandsToStr(instr.operands)
      case Muls => "muls " + operandsToStr(instr.operands)
      case Smull => "smull " + operandsToStr(instr.operands)
      case Sdiv => "sdiv " + operandsToStr(instr.operands)
      case And => "and " + operandsToStr(instr.operands)
      case Cmp => "cmp " + operandsToStr(instr.operands)
      case Adr => "adr " + operandsToStr(instr.operands)
      case Adrp => "adrp " + operandsToStr(instr.operands)

      // Branch Instructions
      case B => "b " + operandsToStr(instr.operands)
      case Beq => "b.eq " + operandsToStr(instr.operands)
      case Bne => "b.ne " + operandsToStr(instr.operands)
      case Bvs => "b.vs " + operandsToStr(instr.operands)
      case Bl => "bl " + operandsToStr(instr.operands)
      case Cbz => "cbz " + operandsToStr(instr.operands)

      // Load and Store Instructions
      case Mov => "mov " + operandsToStr(instr.operands)
      case Ldr => "ldr " + ldrOperandsToStr(instr.operands)
      case Ldur => "ldur " + operandsToStr(instr.operands)
      case Str => "str " + operandsToStr(instr.operands)
      case Stur => "stur " + operandsToStr(instr.operands)
      case Ldrsb => "ldrsb " + operandsToStr(instr.operands)
      case Strb => "strb " + operandsToStr(instr.operands)
      case Stp => "stp " + operandsToStr(instr.operands)
      case StpUpdate => "stp " + operandsToStr(instr.operands) + "!"
      case Ldp => "ldp " + operandsToStr(instr.operands)
      case LdpUpdate => "ldp " + operandsToStr(instr.operands)

      // Conditional Instructions
      case Cselgt => "csel " + operandsToStr(instr.operands) + ", gt"
      case Csellt => "csel " + operandsToStr(instr.operands) + ", lt"
      case Cselge => "csel " + operandsToStr(instr.operands) + ", ge"
      case Cselle => "csel " + operandsToStr(instr.operands) + ", le"
      case Cselne => "csel " + operandsToStr(instr.operands) + ", ne"
      case Cseleq => "csel " + operandsToStr(instr.operands) + ", eq"

      case Csetgt => "cset " + operandsToStr(instr.operands) + ", gt"
      case Csetlt => "cset " + operandsToStr(instr.operands) + ", lt"
      case Csetge => "cset " + operandsToStr(instr.operands) + ", ge"
      case Csetle => "cset " + operandsToStr(instr.operands) + ", le"
      case Csetne => "cset " + operandsToStr(instr.operands) + ", ne"
      case Cseteq => "cset " + operandsToStr(instr.operands) + ", eq"

      // Other Instructions
      case UserFuncLabel(name) => renameFuncLabel(name) + ":"
      case BranchLabel(id) => renameBranchLabel(id) + ":"

      case Lsl => "lsl " + operandsToStr(instr.operands)
      case Tst => "tst " + operandsToStr(instr.operands)
      case Sbfx => "sbfx " + operandsToStr(instr.operands)

      case Ret => "ret"

      case Comment(comment) => "// " + comment
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
      case HexImm(value) => s"#0x$value"
      case reg: Register => registerToStr(reg)
      case Stack(reg, offset) => s"[${operandToStr(reg)}, #${offset}]"
      case StackReg(reg) => s"[${operandToStr(reg)}]"
      case StackOffsetReg(reg, offset) => s"[${operandToStr(reg)}, ${operandToStr(offset)}]"
      case HeapLocImm(reg, offset) => s"[${registerToStr(reg)}, ${operandToStr(offset)}]"
      case HeapLocReg(reg, offset) => s"[${registerToStr(reg)}, ${operandToStr(offset)}]"

      case Reloc(op) => ":lo12:" + operandToStr(op)

      case BranchLabel(id) => s".L${id}"
      case StrLabel(id) => renameStringLabel(id)
      case UserFuncLabel(name) => renameFuncLabel(name)

      case Asr(value) => s"asr #$value"

      case ArrLoad1 => "_arrLoad1"
      case ArrLoad4 => "_arrLoad4"
      case ArrLoad8 => "_arrLoad8"
      case ArrStore1 => "_arrStore1"
      case ArrStore4 => "_arrStore4"
      case ArrStore8 => "_arrStore8"
      case ErrBadChar => "_errBadChar"
      case ErrDivZero => "_errDivZero"
      case ErrNull => "_errNull"
      case ErrOutOfBounds => "_errOutOfBounds"
      case ErrOutOfMemory => "_errOutOfMemory"
      case ErrOverflow => "_errOverflow"
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
      case Free => "free"
      case Exit => "exit"
    }
  }

  // Converts a register to a string.
  private def registerToStr(register: Register): String = {
    register match {
      case X0(X) => "x0"
      case X0(W) => "w0"
      case X1(X) => "x1"
      case X1(W) => "w1"
      case X2(X) => "x2"
      case X2(W) => "w2"
      case X3(X) => "x3"
      case X3(W) => "w3"
      case X4(X) => "x4"
      case X4(W) => "w4"
      case X5(X) => "x5"
      case X5(W) => "w5"
      case X6(X) => "x6"
      case X6(W) => "w6"
      case X7(X) => "x7"
      case X7(W) => "w7"
      case X8(X) => "x8"
      case X8(W) => "w8"
      case X9(X) => "x9"
      case X9(W) => "w9"
      case X10(X) => "x10"
      case X10(W) => "w10"
      case X11(X) => "x11"
      case X11(W) => "w11"
      case X12(X) => "x12"
      case X12(W) => "w12"
      case X13(X) => "x13"
      case X13(W) => "w13"
      case X14(X) => "x14"
      case X14(W) => "w14"
      case X15(X) => "x15"
      case X15(W) => "w15"
      case X16(X) => "x16"
      case X16(W) => "w16"
      case X17(X) => "x17"
      case X17(W) => "w17"
      case X18(X) => "x18"
      case X18(W) => "w18"
      case X19(X) => "x19"
      case X19(W) => "w19"
      case X20(X) => "x20"
      case X20(W) => "w20"
      case X21(X) => "x21"
      case X21(W) => "w21"
      case X22(X) => "x22"
      case X22(W) => "w22"
      case X23(X) => "x23"
      case X23(W) => "w23"
      case X24(X) => "x24"
      case X24(W) => "w24"
      case X25(X) => "x25"
      case X25(W) => "w25"
      case X26(X) => "x26"
      case X26(W) => "w26"
      case X27(X) => "x27"
      case X27(W) => "w27"
      case X28(X) => "x28"
      case X28(W) => "w28"
      case FP => "fp"
      case LR => "lr"
      case SP => "sp"
      case XZR => "xzr"
      case _ => "x8"
    }
  }
}