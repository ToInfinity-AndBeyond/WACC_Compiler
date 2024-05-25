package backendUnitTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.ast.ast._
import wacc.semantic._
import wacc.ast.Types._
import wacc.ir._

/* Test syntax errors in invalid codes 
   Most cases are covered in integration test, 
   so this is for the early stage of parser implementation */

class UnitTest extends AnyFlatSpec with Matchers {

  val usePrintIR = true

  val p: (Int, Int) = (0, 0)

  private def printIR(ir: IR) = {
    if (usePrintIR) {
      println("Strings: ")
      for (str <- ir.strings) {
        print(s"$str, ")
      }
      println("\nFunctions:")
      for(funcIR <- ir.funcs) {
        println(funcIR.name)
        println(funcIR.instrs)
      }
    }
  }

  private def generateSymbolTree(ast: Program): SymbolTree = {
    SemanticCheck.semanticCheck(ast) match {
      case Right(tree) => tree
      case Left(error) => {
        fail("Error in semantic check symbol tree generation")
      }
    }
  }

  private def generateIr(ast: Program, symbolTable: SymbolTree): IR = {
    irBuilder.buildIr(ast, symbolTable) match {
      case Right(ir) => ir
      case Left(error) => {
        fail(error.errorInfos.mkString("\n"))
      }
    }
  }

  private def checkMainExists(ast: Program, ir: IR): Boolean = {
    ir.funcs(0).name == "main"
  }

  private def checkFuncsExist(ast: Program, ir: IR): Boolean = {
    for(astFunc <- ast.funcs) {
      val astFuncName = astFunc.ident.name
      if(!ir.funcs.map(f => f.name).contains(astFuncName)) {
        false
      }
    }
    true
  }

  "irBuilder" should "generate main function in IR" in {
    val testAst: Program = Program(List(), List(Declare(CharType, Ident("a")(p), CharLiter('c')(p))(p)))(p)
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)

    checkMainExists(testAst, ir) shouldBe true
    checkFuncsExist(testAst, ir) shouldBe true
  }

  it should "generate valid IR for plus expressions" in {
    val testAst: Program = TestCases.plusExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)
    
    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "35\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for minus expressions" in {
    val testAst: Program = TestCases.minusExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "5\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for multiplication expressions" in {
    val testAst: Program = TestCases.multExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "15\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for negation expressions" in {
    val testAst: Program = TestCases.negExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "-42\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for multiple negation expressions" in {
    val testAst: Program = TestCases.negExpr2
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "42\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for and expressions" in {
    val testAst: Program = TestCases.andExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)
    
    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "false\ntrue\nfalse\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for or expressions" in {
    val testAst: Program = TestCases.orExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "true\ntrue\nfalse\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for equals expressions" in {
    val testAst: Program = TestCases.equalsExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "false\nfalse\ntrue\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for not equals expressions" in {
    val testAst: Program = TestCases.notequalsExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "true\ntrue\nfalse\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for greater than expressions" in {
    val testAst: Program = TestCases.greaterExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "false\ntrue\n"
    exitCode shouldBe 0
    success shouldBe true
  }

  it should "generate valid IR for int assignment" in {
    val testAst: Program = TestCases.assignExpr
    val symbolTable: SymbolTree = generateSymbolTree(testAst)
    val ir: IR = generateIr(testAst, symbolTable)
    printIR(ir)

    val (output: String, exitCode: Int, success: Boolean) = Main.runEmulation(ir)
    output shouldBe "4\n2\n"
    exitCode shouldBe 0
    success shouldBe true
  }
}
