package backendUnitTest

import scala.collection.immutable
import scala.collection.mutable
import wacc.ir._
import wacc.ir.threeAddrCode._

object Main {

  private class emuState(
    private var input: String,
    val funcs: List[ThreeAddrFunc],
    val stringMap: immutable.Map[String, TStringLabel], 
    val functionMap: immutable.Map[String, Int], 
    val registerMap: mutable.Map[TLoc, TImm],
    val labelMap: immutable.Map[String, ProgramCounter],
    val argumentMap: mutable.Map[Int, TImm] = mutable.Map()
  ) {
    val heap: Heap = new Heap()
    var output: String = ""
    var registerMapStack: mutable.Stack[mutable.Map[TLoc, TImm]] = mutable.Stack()
    // var exitCode: Int = 0

    def getInput() = input
    def getStringMap() = stringMap
    def getFunctionMap() = functionMap
    def getLabelMap() = labelMap

    def addStr(str: String) = {
      output += str
    }
    // def setExitCode(code: Int) = {
    //   exitCode = code
    // }

    def getChar(): Option[Char] = {
      input.headOption match {
        case Some(char) => {
          input = input.tail
          Some(char)
        }
        case None => None
      }
    }
    def getInt(): Option[Int] = {
      val (numStr, remainingStr) = input.span(_.isDigit)
      input = remainingStr.trim
      Some(numStr.toInt)
    }

    // Get a map of argument to immediate value from argLocs
    def getArgumentMap(argLocs: List[TLoc]): Either[ErrorInfo, mutable.Map[Int, TImm]] = {
      var argumentMap: mutable.Map[Int, TImm] = mutable.Map()
      var argCnt = 0

      for(loc <- argLocs) {
        val value = registerMap.get(loc) match {
          case Some(v) => v
          case None => return Left(new ErrorInfo("register not found"))
        }
        argumentMap += (argCnt -> value)
        argCnt += 1
      }

      Right(argumentMap)
    }
  }
  class ErrorInfo(var msg: String) {
    def addMsg(str: String) = {
      msg += str
    }
  }

  private class Heap() {
    private val heap: mutable.Map[Int, Int] = mutable.Map()
    private var nextFree: Int = 0
    def malloc(size: Int): Int = {
      // return memory address as int
      val start = nextFree
      nextFree += size
      start
    }
    def free(start: Int, size: Int) = {
      for (i <- start until start + size) {
        heap -= i
      }
    }
    def load(start: Int, offset: Int): Int = {
      heap(start + offset)
    }
    def store(start: Int, offset: Int, value: Int) = {
      heap(start + offset) = value
    }
  }

  private class ProgramCounter(val func: Int, val line: Int) {
    def increment(): ProgramCounter = {
      new ProgramCounter(func, line + 1)
    }
  }

  private class ExitStatus(val code: Int, val exit: Boolean = false)

  def runEmulation(ir: IR, input: String = ""): (String, Int, Boolean) = {
    // make a string map
    val stringMap: immutable.Map[String, TStringLabel] = ir.strings.map(x => (x.value, x)).toMap

    // make a function map
    val functionMap: immutable.Map[String, Int] = 
      ir.funcs.zipWithIndex.map{ case (func, index) => (func.name, index) }.toMap
    
    // make a register map
    val registerMap: mutable.Map[TLoc, TImm] = mutable.Map()

    val labelMap: immutable.Map[String, ProgramCounter] = 
      ir.funcs.zipWithIndex.foldLeft(Map[String, ProgramCounter]()) { case (map, (func, i)) =>
        func.instrs.zipWithIndex.foldLeft(map) { case (innerMap, (instr, j)) =>
          instr match {
            case l: TBranchLabel => innerMap + (renameBranchLabel(l) -> new ProgramCounter(i, j))
            case f: TFuncLabel => innerMap + (renameFuncLabel(f) -> new ProgramCounter(i, j))
            case _ => innerMap
          }
        }
      }
    
    val emuState = new emuState(input, ir.funcs, stringMap, functionMap, registerMap, labelMap)

    var programCounter = new ProgramCounter(functionMap("main"), 0)
    runFunc(programCounter, emuState) match {
      case Right(exitStatus) => (emuState.output, exitStatus.code, true)
      case Left(err) => (err.msg, -1, false)
    }
  }

  private def runFunc(pc: ProgramCounter, state: emuState): Either[ErrorInfo, ExitStatus] = {
    var curPC: ProgramCounter = pc
    while (curPC.line < state.funcs(curPC.func).instrs.size) {
      curPC = runInstr(curPC, state) match {
        case Left(err) => return Left(err)
        case Right(Right(exitStatus)) => return Right(exitStatus)
        case Right(Left(pc)) => pc
      }
    }
    Right(new ExitStatus(0, false))
  }

  private def runInstr(pc: ProgramCounter, state: emuState): Either[ErrorInfo, Either[ProgramCounter, ExitStatus]] = {
    val instr: ThreeAddrInstr = state.funcs(pc.func).instrs(pc.line)
    instr match {
      case TPrintB(op, newLine) => operandToBool(op, state) match {
        case Right(boolVal) => {
          state.addStr(boolVal.toString)
          if (newLine) state.addStr("\n")
          Right(Left(pc.increment()))
        }
        case Left(error) => Left(error)
      }
      case TPrintC(op, newLine) => operandToChar(op, state) match {
        case Right(charVal) => {
          state.addStr(charVal.toString)
          if (newLine) state.addStr("\n")
          Right(Left(pc.increment()))
        }
        case Left(error) => Left(error)
      }
      case TPrintI(op, newLine) => operandToInt(op, state) match {
        case Right(intVal) => {
          state.addStr(intVal.toString)
          if (newLine) state.addStr("\n")
          Right(Left(pc.increment()))
        }
        case Left(error) => Left(error)
      }
      case TPrintP(loc, newLine) => ???
      case TPrintS(loc, newLine) => ???

      case TReadC(loc) => {
        state.getChar() match {
          case None => Left(new ErrorInfo("No input left"))
          case Some(charVal) => {
            state.registerMap += (loc -> new TImm(charVal.toInt))
            Right(Left(pc.increment()))
          }
        }
        
      }
      case TReadI(loc) => {
        state.getInt() match {
          case None => Left(new ErrorInfo("No input left"))
          case Some(intVal) => {
            state.registerMap += (loc -> new TImm(intVal))
            Right(Left(pc.increment()))
          }
        }
      }
      case TMallocCharArray(dest, size) => ???

      case unOp: TUnOp => runUnOp(unOp, state) match {
        case None => Right(Left(pc.increment()))
        case Some(err) => Left(err)
      }
      case binOp: TBinOp => runBinOp(binOp, state) match {
        case None => Right(Left(pc.increment()))
        case Some(err) => Left(err)
      }
      case TExit(op) => operandToInt(op, state) match {
        case Right(intVal) => {
          Right(Right(new ExitStatus(intVal, true)))
        }
        case Left(err) => Left(err)
      }
      case TFreeA(loc) => operandToInt(loc, state) match {
        case Right(memAddr) => {
          val size = state.heap.load(memAddr-1, 0)
          state.heap.free(memAddr-1, size+1)
          Right(Left(pc.increment()))
        }
        case Left(err) => Left(err)
      }
      case TFreeP(loc) => operandToInt(loc, state) match {
        case Right(memAddr) => {
          state.heap.free(memAddr, 2)
          Right(Left(pc.increment()))
        }
        case Left(err) => Left(err)
      }
      case TCall(func, args, ret) => {
        val argMap = state.getArgumentMap(args) match {
          case Left(err) => return Left(err)
          case Right(newMap) => newMap
        }
        val funcState = new emuState(state.getInput(), state.funcs, state.stringMap, state.getFunctionMap(), mutable.Map(), state.getLabelMap(), argMap)

        val funcIndex = state.functionMap(func)
        val newPC = new ProgramCounter(funcIndex, 0)
        runFunc(newPC, funcState) match {
          case Right(exitStatus) => {
            if (exitStatus.exit) {
              Right(Right(exitStatus))
            } else {
              state.registerMap += (ret -> new TImm(exitStatus.code))
              Right(Left(pc.increment()))
            }
          }
          case Left(err) => Left(err)
        }
      }
      case TReturn(op) => operandToInt(op, state) match {
        case Right(intVal) => {
          Right(Right(new ExitStatus(intVal, false)))
        }
        case Left(err) => Left(err)
      }
      case TJump(label) => state.labelMap.get(renameBranchLabel(label)) match {
        case Some(newPC) => Right(Left(newPC))
        case None => Left(new ErrorInfo("Label not found"))
      }
      case TJumpCond(cond, label) => {
        if (operandToBool(cond, state).toOption.get) {
          state.labelMap.get(renameBranchLabel(label)) match {
            case Some(newPC) => Right(Left(newPC))
            case None => Left(new ErrorInfo("Label not found"))
          }
        } else {
          Right(Left(pc.increment()))
        }
      }

      case TSkip() => Right(Left(pc.increment()))

      case TAssignLoc(dest, op) => {
        operandToInt(op, state) match {
          case Left(value) => Left(value)
          case Right(value) => {
            state.registerMap += (dest -> new TImm(value))
            Right(Left(pc.increment()))
          }
        }
      }
      case TAssignImm(dest, imm) => {
        state.registerMap += (dest -> imm)
        Right(Left(pc.increment()))
      }
      // case TAssignArg(dest, arg) => {
      //   val imm = state.argumentMap.get(arg.id) match {
      //     case Some(value) => value
      //     case None => return Left(new ErrorInfo(s"Argument ($arg) not found"))
      //   }
      //   state.registerMap += (dest -> imm)
      //   Right(Left(pc.increment()))
      // }
      // case TLabel(label) => Right(Left(pc.increment()))
      case TMallocArray(retLoc, size) => state.heap.malloc(size + 1) match {
        case ret => {
          state.heap.store(ret, 0, size+1)
          state.registerMap += (retLoc -> new TImm(ret+1))
          Right(Left(pc.increment()))
        }
      }
      case TMallocPair(retLoc) => state.heap.malloc(2) match {
        case ret => {
          state.registerMap += (retLoc -> new TImm(ret))
          Right(Left(pc.increment()))
        }
      }
      case TBranchLabel(id) => Right(Left(pc.increment()))
      case TFuncLabel(name) => Right(Left(pc.increment()))
    }
  }

  private def operandToChar(op: TLoc, state: emuState): Either[ErrorInfo, Char] = {
    operandToInt(op, state) match {
      case Right(intVal) => Right(intVal.toChar)
      case Left(error) => Left(error)
    }
  }

  private def operandToBool(op: TLoc, state: emuState): Either[ErrorInfo, Boolean] = {
    operandToInt(op, state) match {
      case Right(intVal) => Right(intVal != 0)
      case Left(error) => Left(error)
    }
  }

  private def operandToInt(op: TLoc, state: emuState): Either[ErrorInfo, Int] = {
    if (state.registerMap.get(op).isDefined) {
      Right(state.registerMap(op).value)
    } else {
      Left(new ErrorInfo("Register not found"))
    }
  }

  private def runUnOp(unOp: TUnOp, state: emuState): Option[ErrorInfo] = {
    unOp match {
      case TNOT(dest, value) => {
        val v = operandToBool(value, state)
        if (v.isRight) {
          state.registerMap += (dest -> new TImm(if (v.toOption.get) 0 else 1))
          None
        } else {
          Some(new ErrorInfo("Error in NOT"))
        }
      }
      case TNEG(dest, value) => {
        val v = operandToInt(value, state)
        if (v.isRight) {
          state.registerMap += (dest -> new TImm(-v.toOption.get))
          None
        } else {
          Some(new ErrorInfo("Error in NEG"))
        }
      }
      case TCHR(dest, value) => {
        val v = operandToInt(value, state)
        if (v.isRight) {
          state.registerMap += (dest -> new TImm(v.toOption.get))
          None
        } else {
          Some(new ErrorInfo("Error in CHR"))
        }
      }
      case TORD(dest, value) => {
        val v = operandToChar(value, state)
        if (v.isRight) {
          state.registerMap += (dest -> new TImm(v.toOption.get.toInt))
          None
        } else {
          Some(new ErrorInfo("Error in ORD"))
        }
      }
      case TLEN(dest, value) => ???
    }
  }

  private def runBinOp(binOp: TBinOp, state: emuState): Option[ErrorInfo] = {
    binOp match {
      case TADD(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(lval.toOption.get + rval.toOption.get))
          None
        } else {
          Some(new ErrorInfo("Error in ADD"))
        }
      }
      case TMOD(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(lval.toOption.get % rval.toOption.get))
          None
        } else {
          Some(new ErrorInfo("Error in MOD"))
        }
      }
      case TDIV(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(lval.toOption.get / rval.toOption.get))
          None
        } else {
          Some(new ErrorInfo("Error in DIV"))
        }
      }
      case TSUB(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(lval.toOption.get - rval.toOption.get))
          None
        } else {
          Some(new ErrorInfo("Error in SUB"))
        }
      }
      case TMUL(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(lval.toOption.get * rval.toOption.get))
          None
        } else {
          Some(new ErrorInfo("Error in MUL"))
        }
      }
      case TGT(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(if (lval.toOption.get > rval.toOption.get) 1 else 0))
          None
        } else {
          Some(new ErrorInfo("Error in GT"))
        }
      }
      case TGTE(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(if (lval.toOption.get >= rval.toOption.get) 1 else 0))
          None
        } else {
          Some(new ErrorInfo("Error in GTE"))
        }
      }
      case TLT(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(if (lval.toOption.get < rval.toOption.get) 1 else 0))
          None
        } else {
          Some(new ErrorInfo("Error in LT"))
        }
      }
      case TLTE(dest, lvalue, rvalue) => {
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(if (lval.toOption.get <= rval.toOption.get) 1 else 0))
          None
        } else {
          Some(new ErrorInfo("Error in LTE"))
        }
      }
      case TEQ(dest, lvalue, rvalue) => {
        // not implemented fully
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(if (lval.toOption.get == rval.toOption.get) 1 else 0))
          None
        } else {
          Some(new ErrorInfo("Error in EQ"))
        }
      }
      case TNEQ(dest, lvalue, rvalue) => {
        // not implemented fully
        val lval = operandToInt(lvalue, state)
        val rval = operandToInt(rvalue, state)
        if (lval.isRight && rval.isRight) {
          state.registerMap += (dest -> new TImm(if (lval.toOption.get != rval.toOption.get) 1 else 0))
          None
        } else {
          Some(new ErrorInfo("Error in NEQ"))
        }
      }
      
      // case TArrLoad(locDest, arr, index) => {
      //   val arrLoc = state.registerMap(arr).value
      //   val idx = operandToInt(index, state).toOption.get
      //   val value = state.heap.load(arrLoc, idx)
      //   state.registerMap += (locDest -> new TImm(value))
      //   None
      // }
      case TLoad(locDest, memorySrc, offset) => {
        val value = state.heap.load(state.registerMap(memorySrc).value, operandToInt(offset, state).toOption.get)
        state.registerMap += (locDest -> new TImm(value))
        None
      }
      case TStore(value, memoryDest, offset) => {
        state.heap.store(state.registerMap(memoryDest).value, operandToInt(offset, state).toOption.get, state.registerMap(value).value)
        None
      }
      case TCharStore(srcTLoc, memoryDest, offset) => {
        state.heap.store(state.registerMap(memoryDest).value, operandToInt(offset, state).toOption.get, state.registerMap(srcTLoc).value)
        None
      }
      // case TCharLoad(destTLoc, memorySrc, offset) => {
      //   val value = state.heap.load(state.registerMap(memorySrc).value, operandToInt(offset, state).toOption.get)
      //   state.registerMap += (destTLoc -> new TImm(value))
      //   None
      // }
      // from here are not verified
      case TCharArrElemStore(srcTLoc, memoryDest, offset) => {
        val value = state.registerMap(srcTLoc).value
        state.heap.store(state.registerMap(memoryDest).value, operandToInt(offset, state).toOption.get, value)
        None
      }
      case TArrElemStore(srcTLoc, memoryDest, offset) => {
        val value = state.registerMap(srcTLoc).value
        state.heap.store(state.registerMap(memoryDest).value, operandToInt(offset, state).toOption.get, value)
        None
      }
      case TPairElemStore(srcTLoc, memoryDest, offset) => {
        val value = state.registerMap(srcTLoc).value
        state.heap.store(state.registerMap(memoryDest).value, operandToInt(offset, state).toOption.get, value)
        None
      }
      case TCharArrElemLoad(destTLoc, memorySrc, offset) => {
        val value = state.heap.load(state.registerMap(memorySrc).value, operandToInt(offset, state).toOption.get)
        state.registerMap += (destTLoc -> new TImm(value))
        None
      }
      case TArrElemLoad(destTLoc, memorySrc, offset) => {
        val value = state.heap.load(state.registerMap(memorySrc).value, operandToInt(offset, state).toOption.get)
        state.registerMap += (destTLoc -> new TImm(value))
        None
      }
      case TPairElemLoad(destTLoc, memorySrc, offset) => {
        val value = state.heap.load(state.registerMap(memorySrc).value, operandToInt(offset, state).toOption.get)
        state.registerMap += (destTLoc -> new TImm(value))
        None
      }
      case TStrLoad(loc, str) => ??? // not implemented
      // case TADDOffset(dest, lValue, offset) => ???
    }
  }
  private def renameBranchLabel(branchLabel: TBranchLabel): String = {
    "L" + branchLabel.id
  }
  private def renameFuncLabel(funcLabel: TFuncLabel): String = {
    "wacc_" + funcLabel.name
  }
}