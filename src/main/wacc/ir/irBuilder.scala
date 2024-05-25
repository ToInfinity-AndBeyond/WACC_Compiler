package wacc.ir

import wacc.ast.ast.Program
import wacc.errors.WaccError.{WaccErrorReport, UnexpectedError}
import wacc.ir.threeAddrCode.TStringLabel
import wacc.semantic.SymbolTree
import statementTranslator.{translateMain, translateFunctions}

object irBuilder {
  // Entry point for the IR generation
  def buildIr(ast: Program, symbolTree: SymbolTree): Either[WaccErrorReport, IR] = {
    try {
      val state: State = new State(symbolTree)
      val funcs: List[ThreeAddrFunc] = {
        val fs: List[ThreeAddrFunc] = translateFunctions(ast.funcs, state)
        val main: ThreeAddrFunc = translateMain(ast.stmts, state)
        main :: fs
      }
      val strings: List[TStringLabel] = state.stringTable.toStringList()
      Right(new IR(strings, funcs))
    } catch {
      case e: Exception => {
        e.printStackTrace()
        Left(new UnexpectedError("IR generation failed" + e.getMessage()))
      }
    }
  }
}