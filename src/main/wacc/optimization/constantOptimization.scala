package wacc.optimization

import wacc.ast.Types._
import wacc.ast.ast._
import wacc.errors.WaccError._
import wacc.semantic.SymbolTree
import wacc.optimization.ConstantState
import constantFolding.expressionToConstant
import constantPropagation.optimizeFunction
import constantPropagation.optimizeMain

import scala.collection.mutable
import wacc.ir.expressionTranslator
import wacc.syntax.lexer

object constantOptimization {
  private final val p = (0, 0)

  /* Optimize program with constant propagation and control flow analysis */
  def constantOptimization(ast: Program, symbolTree: SymbolTree): Program = {
    val state = new ConstantState(symbolTree, new ConstantMap(mutable.Map()))

    val fs: List[Func] = ast.funcs.map(optimizeFunction(_, state))

    val mainStmts = optimizeMain(ast.stmts, state)
    
    Program(fs, mainStmts)(p) // Position is not used after Semantic check, so set as 0,0
  }
}
