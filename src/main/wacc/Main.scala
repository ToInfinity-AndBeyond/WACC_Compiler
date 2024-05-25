package wacc

import scala.io.Source
import ast.ast.Program
import syntax.parser
import errors.PrettyPrint.prettyPrint
import semantic.SemanticCheck.semanticCheck
import semantic.SymbolTree
import errors.WaccError._
import java.io.{PrintWriter, File, FileWriter}
import java.nio.file.Paths
import ir.irBuilder.buildIr
import optimization.constantOptimization
import codegen._
import ExitCodes._

import wacc.ir.statementTranslator
import wacc.codegen.arm32.arm32Instr

import wacc.ir.IR


object ExitCodes {
  val SUCCESS_CODE        = 0
  val SYNTAX_ERROR_CODE   = 100
  val SEMANTIC_ERROR_CODE = 200
  val CONSTANT_OPTIMIZATION_ERROR_CODE = 150
  val FILE_ERROR_CODE     = 1
  val UNEXPECTED_ERROR_CODE = 1
}

object Main {
  // Whether or not to run the constant optimization step
  // private final val useConstantOptimization = true

  private final val WACC_TARGET: String = "wacc.target"
  // Runs the given file through the parser and semantic checker
  // Returns a tuple of an optional list of WaccErrors and an exit code
  def runFile(srcFile: String, destFile: String, target: TargetArchitecture, runOptimzation: Boolean = true): (Option[WaccErrorReport], Int) = {
    // run syntax check, semantic check, and code generation in order
    runSyntaxCheck(srcFile) match {
      case Left((err, exitCode)) => (Some(err), exitCode)
      case Right(program) => runSemanticCheck(program, srcFile) match {
        case Left((err, exitCode)) => (Some(err), exitCode)
        case Right(symbolTree) => {
          if (runOptimzation) {
            // Go through optimization
            runConstantOptimization(program, srcFile, symbolTree) match {
              case Left((err, exitCode)) => (Some(err), exitCode)
              case Right(optProgram) => codeGeneration(optProgram, symbolTree, destFile, target)
            }
          }
          else
            codeGeneration(program, symbolTree, destFile, target)
        }
      }
    }
  }

  // Runs the syntax check on the given file
  // Returns either a tuple (WaccErrorReport, exit code) or the AST
  private def runSyntaxCheck(filename: String): Either[(WaccErrorReport, Int), Program] = {
    parser.parseFile(new File(filename)) match {
      case scala.util.Failure(exception) => 
        Left((new FileNotFound(filename), FILE_ERROR_CODE))
      case scala.util.Success(ast) => ast match {
        case parsley.Failure(waccError) => 
          Left((new WaccErrorReport(filename, List(waccError)), SYNTAX_ERROR_CODE))
        case parsley.Success(program) => Right(program)
      }
    }
  }

  // Runs the semantic checker on the given AST
  // Returns a tuple of an optional list of WaccErrors and an exit code
  private def runSemanticCheck(ast: Program, filename: String): Either[(WaccErrorReport, Int), SymbolTree] = {
    semanticCheck(ast) match {
      case Left(errs) => Left(new WaccErrorReport(filename, errs), SEMANTIC_ERROR_CODE)
      case Right(symbolTree) => Right(symbolTree)
    }
  }

  private def runConstantOptimization(ast: Program, filename: String, symbolTree: SymbolTree): Either[(WaccErrorReport, Int), Program] = {
    try {
      Right(constantOptimization.constantOptimization(ast, symbolTree))
    } catch {
      // Check for runtime errors caught during compilation
      // This includes integer overflow, character overflow, divide by 0
      case e: RuntimeException => {
        val pattern = """^(.*?):(.*?)\s*\((\d+),(\d+)\)$""".r

        println(e.getMessage())
        e.getMessage() match {
          case pattern(errType, reason, int1, int2) =>
            val int1Val = int1.toInt
            val int2Val = int2.toInt
            Left((new WaccErrorReport(filename, List(new OtherError(errType, None, None, None, Some(reason), (int1Val, int2Val)))), SEMANTIC_ERROR_CODE))
          case _ =>
            throw new RuntimeException(e.getMessage()) // Problem with the optimizer
        }
      }
    }
  }

  private def printIR(ir: IR) = {
    println("Strings: ")
    for (str <- ir.strings) {
      print(s"$str, ")
    }
    println("\nFunctions:")
    for (funcIR <- ir.funcs) {
      println(funcIR.name)
      println(funcIR.instrs)
    }
  }

  private def codeGeneration(ast: Program, symbolTree: SymbolTree, destFile: String, target: TargetArchitecture): (Option[WaccErrorReport], Int) = {
    buildIr(ast, symbolTree) match {
      case Left(err) => (Some(err), UNEXPECTED_ERROR_CODE)
      case Right(ir) => 
        codeGenerator.generateCode(ir, destFile, target) match {
        case Some(err) => (Some(err), UNEXPECTED_ERROR_CODE)
        case None => (None, SUCCESS_CODE)
      }
    }
  }

  private def getTargetArchitecture(): TargetArchitecture = {
    try {
      val source = Source.fromFile(WACC_TARGET)
      val contents = source.mkString
      source.close()
      contents match {
        case "aarch64" => AARCH64
        case "arm32" => ARM32
        case "x86-64" => X86_64
        case _ => ARM32
      }
    } catch {
      case e: Throwable =>
        println(s"Configuration file $WACC_TARGET not found, defaulting to AARCH64")
        AARCH64
    }
  }

  def main(args: Array[String]): Unit = {
    // Read command line arguments (file name) and pass as a file object
    // example filename: src/test/wacc/skip.wacc
    val (result, exitCode) = args match {
      case Array(srcFile, "noopt") => {
        val destFile: String = Paths.get(srcFile).getFileName.toString().replace(".wacc", ".s")
        val target: TargetArchitecture = getTargetArchitecture()

        println("No Optimization")
        runFile(srcFile, destFile, target, false)
      }
      case Array(srcFile) => {
        val destFile: String = Paths.get(srcFile).getFileName.toString().replace(".wacc", ".s")
        val target: TargetArchitecture = getTargetArchitecture()

        println("Running with Optimization")
        runFile(srcFile, destFile, target)
      }
      case _ => (Some(ArgumentEmpty), FILE_ERROR_CODE)
    }

    result match {
      case None => println("Compilation successful")
      case Some(errorReport) => prettyPrint(errorReport)
    }
    
    sys.exit(exitCode)
  }

}
