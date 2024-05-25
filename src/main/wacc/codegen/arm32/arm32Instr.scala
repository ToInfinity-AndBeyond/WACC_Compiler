package wacc.codegen.arm32

object arm32Instr {
  // arm32Func represents an ARM32 function with a name, a list of instructions, and a ltorg flag
  case class arm32Func(val name: String, val instrs: List[Instr], val ltorg: Boolean)

  // Instr represents an ARM32 instruction with an opcode and a list of operands
  case class Instr(val opcode: OpCode, val operands: List[Operand])

  // OpCode is a trait for all possible ARM32 opcodes
  sealed trait OpCode

  // Various case objects extending OpCode represent specific ARM32 opcodes
  case object Push extends OpCode
  case object Pop extends OpCode
  case object Ldr extends OpCode
  case object Str extends OpCode
  case object Strb extends OpCode
  case object Ldrsb extends OpCode

  case object B extends OpCode
  case object Beq extends OpCode
  case object Bne extends OpCode
  case object Bl extends OpCode
  case object Blne extends OpCode
  case object Bleq extends OpCode
  case object Blvs extends OpCode

  case object Mov extends OpCode
  case object Movgt extends OpCode
  case object Movlt extends OpCode
  case object Movge extends OpCode
  case object Movle extends OpCode
  case object Movne extends OpCode
  case object Moveq extends OpCode

  case object Add extends OpCode
  case object Adds extends OpCode
  case object Sub extends OpCode
  case object Subs extends OpCode
  case object Rsbs extends OpCode //reverse subtract
  case object Mul extends OpCode
  case object Muls extends OpCode
  case object Smull extends OpCode

  case object And extends OpCode
  case object Cmp extends OpCode
  case object Tst extends OpCode
  case object Lsl extends OpCode

  // Comment represents a comment in the code
  case class Comment(val comment: String) extends OpCode

  // Operand is a trait for all possible operands in an ARM32 instruction
  sealed trait Operand
  case class Imm(value: Int) extends Operand
  case class Asr(value: Int) extends Operand

  sealed trait StackRegister extends Operand
  sealed trait HeapLocation extends Operand
  case class HeapLocImm(reg: Register, offset: Imm) extends HeapLocation
  case class HeapLocReg(reg: Register, offset: Register) extends HeapLocation

  sealed class Register(val id: Int) extends StackRegister
  case object R0 extends Register(0)
  case object R1 extends Register(1)
  case object R2 extends Register(2)
  case object R3 extends Register(3)
  case object R4 extends Register(4)
  case object R5 extends Register(5)
  case object R6 extends Register(6)
  case object R7 extends Register(7)
  case object R8 extends Register(8)
  case object R9 extends Register(9)
  case object R10 extends Register(10)
  case object FP extends Register(11) // Frame pointer
  case object R12 extends Register(12) // Intra-Procedure-call scratch register
  case object IP extends Register(12) // Intra-Procedure-call scratch register
  case object SP extends Register(13) // Stack pointer
  case object LR extends Register(14) // Link register
  case object PC extends Register(15) // Program counter

  // case class Stack(val reg: Register, val fp: Register, val offset: Int) extends StackRegister
  case class Stack(val reg: Register, val offset: Int) extends StackRegister

  // Label is a trait for all possible labels in an ARM32 instruction
  sealed trait Label extends Operand
  case class StrLabel(id: Int) extends Label
  case class BranchLabel(id: Int) extends Label with OpCode

  sealed trait FuncLabel extends Label
  case class UserFuncLabel(name: String) extends FuncLabel with OpCode
  case object DivMod                 extends FuncLabel

  sealed trait Dependency extends Label

  // Various case objects extending Dependency represent specific dependencies in the code
  case object ArrLoad1               extends Dependency
  case object ArrLoad4               extends Dependency
  case object ArrStore1              extends Dependency
  case object ArrStore4              extends Dependency
  case object ErrBadChar             extends Dependency
  case object ErrDivZero             extends Dependency
  case object ErrNull                extends Dependency
  case object ErrOutOfBounds         extends Dependency
  case object ErrOutOfMemory         extends Dependency
  case object ErrOverflow            extends Dependency
  case object Exit                   extends Dependency
  case object Free                   extends Dependency
  case object FreePair               extends Dependency
  case object Malloc                 extends Dependency
  case object PrintBoolean           extends Dependency
  case object PrintChar              extends Dependency
  case object PrintInteger           extends Dependency
  case object Println                extends Dependency
  case object PrintPointer           extends Dependency
  case object PrintString            extends Dependency
  case object ReadChar               extends Dependency
  case object ReadInteger            extends Dependency
}
