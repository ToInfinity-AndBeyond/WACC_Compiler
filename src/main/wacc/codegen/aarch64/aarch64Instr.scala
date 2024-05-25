package wacc.codegen.aarch64

object aarch64Instr {
  // arm32Func represents an ARM32 function with a name, a list of instructions
  case class arm32Func(val name: String, val instrs: List[Instr])

  // Instr represents an ARM32 instruction with an opcode and a list of operands
  case class Instr(val opcode: OpCode, val operands: List[Operand])

  // OpCode is a trait for all possible ARM32 opcodes
  sealed trait OpCode

  // Various case objects extending OpCode represent specific ARM32 opcodes
  case object Ldr extends OpCode
  case object Ldur extends OpCode
  case object Adr extends OpCode
  case object Adrp extends OpCode
  case object Str extends OpCode
  case object Stur extends OpCode
  case object Stp extends OpCode
  case object StpUpdate extends OpCode
  case object Ldp extends OpCode
  case object LdpUpdate extends OpCode

  case object Strb extends OpCode
  case object Ldrsb extends OpCode

  case object B extends OpCode
  case object Beq extends OpCode
  case object Bne extends OpCode
  case object Bvs extends OpCode
  case object Bl extends OpCode
  case object Cbz extends OpCode

  case object Mov extends OpCode

  case object Cselgt extends OpCode
  case object Csellt extends OpCode
  case object Cselge extends OpCode
  case object Cselle extends OpCode
  case object Cselne extends OpCode
  case object Cseleq extends OpCode

  case object Csetgt extends OpCode
  case object Csetlt extends OpCode
  case object Csetge extends OpCode
  case object Csetle extends OpCode
  case object Csetne extends OpCode
  case object Cseteq extends OpCode

  case object Add extends OpCode
  case object Adds extends OpCode
  case object Sub extends OpCode
  case object Subs extends OpCode
  case object Msub extends OpCode
  case object Mul extends OpCode
  case object Muls extends OpCode
  case object Smull extends OpCode
  case object Sdiv extends OpCode

  case object And extends OpCode
  case object Cmp extends OpCode
  case object Sbfx extends OpCode
  case object Tst extends OpCode
  case object Lsl extends OpCode
  
  case object Ret extends OpCode

  // Comment represents a comment in the code
  case class Comment(val comment: String) extends OpCode

  // Operand is a trait for all possible operands in an ARM32 instruction
  sealed trait Operand
  case class Imm(value: Int) extends Operand
  case class HexImm(value: String) extends Operand
  case class Asr(value: Int) extends Operand
  case class Reloc(op: Operand) extends Operand

  sealed trait StackRegister extends Operand
  sealed trait HeapLocation extends Operand
  case class HeapLocImm(reg: Register, offset: Imm) extends HeapLocation
  case class HeapLocReg(reg: Register, offset: Register) extends HeapLocation

  sealed class RegSize(val bits: Int)
  case object X extends RegSize(64)
  case object W extends RegSize(32)

  sealed class Register(val id: Int) extends StackRegister {
    def getSize(): RegSize = {
      this match{
        case X0(size) => size
        case X1(size) => size
        case X2(size) => size
        case X3(size) => size
        case X4(size) => size
        case X5(size) => size
        case X6(size) => size
        case X7(size) => size
        case X8(size) => size
        case X9(size) => size
        case X10(size) => size
        case X11(size) => size
        case X12(size) => size
        case X13(size) => size
        case X14(size) => size
        case X15(size) => size
        case X16(size) => size
        case X17(size) => size
        case X18(size) => size
        case X19(size) => size
        case X20(size) => size
        case X21(size) => size
        case X22(size) => size
        case X23(size) => size
        case X24(size) => size
        case X25(size) => size
        case X26(size) => size
        case X27(size) => size
        case X28(size) => size
        case _ => X
      }
    }
    def setSize(newSize: RegSize): Register = {
      this match{
        case X0(_) => X0(newSize)
        case X1(_) => X1(newSize)
        case X2(_) => X2(newSize)
        case X3(_) => X3(newSize)
        case X4(_) => X4(newSize)
        case X5(_) => X5(newSize)
        case X6(_) => X6(newSize)
        case X7(_) => X7(newSize)
        case X8(_) => X8(newSize)
        case X9(_) => X9(newSize)
        case X10(_) => X10(newSize)
        case X11(_) => X11(newSize)
        case X12(_) => X12(newSize)
        case X13(_) => X13(newSize)
        case X14(_) => X14(newSize)
        case X15(_) => X15(newSize)
        case X16(_) => X16(newSize)
        case X17(_) => X17(newSize)
        case X18(_) => X18(newSize)
        case X19(_) => X19(newSize)
        case X20(_) => X20(newSize)
        case X21(_) => X21(newSize)
        case X22(_) => X22(newSize)
        case X23(_) => X23(newSize)
        case X24(_) => X24(newSize)
        case X25(_) => X25(newSize)
        case X26(_) => X26(newSize)
        case X27(_) => X27(newSize)
        case X28(_) => X28(newSize)
        case reg => reg
      }
    }
  }
  case class X0(size: RegSize) extends Register(0)
  case class X1(size: RegSize) extends Register(1)
  case class X2(size: RegSize) extends Register(2)
  case class X3(size: RegSize) extends Register(3)
  case class X4(size: RegSize) extends Register(4)
  case class X5(size: RegSize) extends Register(5)
  case class X6(size: RegSize) extends Register(6)
  case class X7(size: RegSize) extends Register(7)
  case class X8(size: RegSize) extends Register(8)
  case class X9(size: RegSize) extends Register(9)
  case class X10(size: RegSize) extends Register(10)
  case class X11(size: RegSize) extends Register(11)
  case class X12(size: RegSize) extends Register(12)
  case class X13(size: RegSize) extends Register(13)
  case class X14(size: RegSize) extends Register(14)
  case class X15(size: RegSize) extends Register(15)
  case class X16(size: RegSize) extends Register(16) // Intra-Procedure-call scratch register
  case class X17(size: RegSize) extends Register(17) // Intra-Procedure-call scratch register
  case class X18(size: RegSize) extends Register(18)
  case class X19(size: RegSize) extends Register(19)
  case class X20(size: RegSize) extends Register(20)
  case class X21(size: RegSize) extends Register(21)
  case class X22(size: RegSize) extends Register(22)
  case class X23(size: RegSize) extends Register(23)
  case class X24(size: RegSize) extends Register(24)
  case class X25(size: RegSize) extends Register(25)
  case class X26(size: RegSize) extends Register(26)
  case class X27(size: RegSize) extends Register(27)
  case class X28(size: RegSize) extends Register(28)
  case object FP extends Register(29) // Frame pointer
  case object LR extends Register(30) // Link register
  case object SP extends Register(31) // Stack pointer
  case object XZR extends Register(32)

  // case class Stack(val reg: Register, val fp: Register, val offset: Int) extends StackRegister
  case class Stack(val reg: Register, val offset: Int) extends StackRegister
  case class StackReg(val reg: Register) extends StackRegister
  case class StackOffsetReg(val reg: Register, val offset: Register) extends StackRegister

  // Label is a trait for all possible labels in an ARM32 instruction
  sealed trait Label extends Operand
  case class StrLabel(id: Int) extends Label
  case class BranchLabel(id: Int) extends Label with OpCode

  sealed trait FuncLabel extends Label
  case class UserFuncLabel(name: String) extends FuncLabel with OpCode
  case object DivMod                 extends FuncLabel
  case object Free                   extends FuncLabel
  case object Exit                   extends FuncLabel

  sealed trait Dependency extends Label

  sealed trait ValType
  case object Char extends ValType
  case object Pointer extends ValType
  case object Generic extends ValType

  // Various case objects extending Dependency represent specific dependencies in the code
  case object ArrLoad1               extends Dependency
  case object ArrLoad4               extends Dependency
  case object ArrLoad8               extends Dependency
  case object ArrStore1              extends Dependency
  case object ArrStore4              extends Dependency
  case object ArrStore8              extends Dependency
  case object ErrBadChar             extends Dependency
  case object ErrDivZero             extends Dependency
  case object ErrNull                extends Dependency
  case object ErrOutOfBounds         extends Dependency
  case object ErrOutOfMemory         extends Dependency
  case object ErrOverflow            extends Dependency
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
