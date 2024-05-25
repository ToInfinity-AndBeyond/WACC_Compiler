package integrationTest

import FileHandler._
import ANSIColors._

import java.io.ByteArrayOutputStream
import java.io.{PrintWriter, File, FileWriter}
import java.nio.file.{StandardCopyOption, Files, Paths, Path}
import scala.concurrent.blocking
import scala.io.Source
import scala.jdk.CollectionConverters._
import wacc.errors.PrettyPrint
import wacc.codegen.TargetArchitecture
import wacc.codegen._

import TestConfig._
import BuildCommands._

object ANSIColors {
  val ANSI_RESET = "\u001B[0m"
  val ANSI_RED   = "\u001B[31m"
  val ANSI_GREEN = "\u001B[32m"
}

object Main {
  def runDirectory(
    srcDir: String,
    destDir: String,
    target: TargetArchitecture,
    emulate: Boolean,
    refCompiler: Boolean = false,
    refEmulator: Boolean = false, 
    ignoreFiles: List[String] = List()
  ): Boolean = {
    initDirectory(destDir)

    val srcFiles: List[String] = getWaccFiles(srcDir)
    var successCount: Int = 0;
    for (testFile <- srcFiles) {
      val destFile = destDir + "/" + testFile
      val srcFile: String = {
        val testPath = srcDir + "/" + testFile
        if (COPY_SRC_FILES_TO_DEST) {
          Files.createDirectories(Paths.get(destFile).getParent)
          Files.copy(Paths.get(testPath), Paths.get(destFile))
          destFile
        } else {
          testPath
        }
      }
      val filePaths: FilePaths = new FilePaths(srcFile, destFile)
      var (success, result) = runFile(filePaths, target, emulate, refCompiler, refEmulator)

      if (ignoreFiles.contains(testFile)) {
        success = true
      }

      // Process test result
      if (success) {
        if (!OUTPUT_ONLY_FAILURE) println(s"$ANSI_GREEN $testFile successful $ANSI_RESET")
        successCount += 1;
      } else {
        println(s"$ANSI_RED $testFile failed $ANSI_RESET")
      }

      // Output test results
      if (OUTPUT_TO_FILE) result.outputToFile(filePaths.resultFile)
      if (OUTPUT_TO_TERMINAL) {
        if (!OUTPUT_ONLY_FAILURE || !success) result.outputToTerminal()
      }
    }
    if (successCount == srcFiles.length) println(s"$ANSI_GREEN All $successCount/${srcFiles.length} tests successful $ANSI_RESET")
    else println(s"$ANSI_RED ${srcFiles.length-successCount}/${srcFiles.length} tests failed $ANSI_RESET")
    successCount == srcFiles.length
  }

  private def runFile(filePaths: FilePaths, target: TargetArchitecture, emulate: Boolean, refCompiler: Boolean, refEmulator: Boolean): (Boolean, TestResult) = {
    val exp: ExpectedOutput = new ExpectedOutput(filePaths.srcFile, target)
    val result: TestResult = new TestResult(filePaths.srcFile)

    val mainSuccess: Boolean = 
      if (refCompiler) runRefCompiler(filePaths, exp, result, target)
      else runCompile(filePaths, exp, result, target)

    val success = 
      if (!exp.compile_error && mainSuccess && emulate) {
        if (refEmulator) runRefEmulator(filePaths, exp, result, target)
        else runAssembly(filePaths, exp, result, target) 
      }
      else mainSuccess
    (success, result)
  }
}

class TestResult(val filePath: String) {
  var compilerResult: Option[String] = None
  var builderResult: Option[String] = None
  var executeResult: Option[String] = None
  var failReason: Option[String] = None

  def addCompilerResult(result: String): Unit = {
    compilerResult = Some(result)
  }

  def addBuilderResult(result: String): Unit = {
    builderResult = Some(result)
  }

  def addExecuteResult(result: String): Unit = {
    if (executeResult.isDefined) executeResult = executeResult.map(_ + result)
    else executeResult = Some(result)
  }

  def addFailReason(failure: String): Unit = {
    failReason = Some(failure)
  }

  private def optionToString(prefix: String, option: Option[String]): String = {
    option match {
      case Some(value) => s"$prefix$value\n"
      case None => ""
    }
  }

  def outputToTerminal(): Unit = {
    val content: String = optionToString("Failure reason: ", failReason) +
      optionToString("Compiler Result:\n", compilerResult) +
      optionToString("Builder Result:\n", builderResult) +
      optionToString("Execute Result\n", executeResult)
    
    print(content)
  }

  def outputToFile(resultFile: String): Unit = {
    val content: String = s"Test Result for $filePath\n" +
      optionToString("Failure reason:\n\t", failReason) +
      optionToString("Compiler Result:\n", compilerResult) +
      optionToString("Builder Result:\n", builderResult) +
      optionToString("Execute Result:\n\t", executeResult)
    
    val writer = new PrintWriter(new FileWriter(resultFile, false))
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
  }
}

class ExpectedOutput(inputFile: String, val target: TargetArchitecture) {
  private val lines: List[String] = Source.fromFile(inputFile).getLines().toList
  val input: Array[Byte] = getInput(lines)
  val output: List[String] = getOutput(lines)
  val exit: Int = getExitCode(lines)
  val undefined_behaviour: Boolean = output.exists(_.contains("#undefined_behaviour#"))
  val compile_error: Boolean = {
    output.headOption match {
      case Some(str) => if (str.contains("#syntax_error#") || str.contains("#semantic_error")) true else false
      case _ => false
    }
}
  private def getInput(lines: List[String]): Array[Byte] = {
    val inputIndex = lines.indexWhere(_.trim.startsWith("# Input:"))
    if (inputIndex != -1) {
      val inputLine = lines(inputIndex)
      val inputContent = inputLine.stripPrefix("# Input:").trim
      inputContent.getBytes
    } else {
      Array.emptyByteArray
    }
  }

  private def getOutput(lines: List[String]): List[String] = {
    lines.dropWhile(_.trim != "# Output:").drop(1).takeWhile(_.nonEmpty)
      .map(_.stripPrefix("#").stripPrefix(" ").trim)
  }

  private def getExitCode(lines: List[String]): Int = {
    val exitIndex = lines.indexWhere(_.trim == "# Exit:")
    if (exitIndex != -1 && lines.isDefinedAt(exitIndex + 1)) {
      lines(exitIndex + 1).stripPrefix("# ").trim.toIntOption.getOrElse(0)
    }
    else 0
  }

  def exitEquals(actualExit: Int): Boolean = {
    if (undefined_behaviour) true
    else exit == actualExit
  }

  private def matchString(expected: String, actual: String): Boolean = {
    def escapeRegexSpecialCharacters(input: String): String = {
      input.replace("\\", "\\\\")
         .replace(".", "\\.")
         .replace("^", "\\^")
         .replace("$", "\\$")
         .replace("|", "\\|")
         .replace("?", "\\?")
         .replace("*", "\\*")
         .replace("+", "\\+")
         .replace("(", "\\(")
         .replace(")", "\\)")
         .replace("{", "\\{")
         .replace("}", "\\}")
         .replace("[", "\\[")
         .replace("]", "\\]")
    }
    val escapedRegex = escapeRegexSpecialCharacters(expected).replace("#runtime_error#", "fatal error: .*")
    val expectedRegex = target match {
      case AARCH64 => escapedRegex.replace("#addrs#", "0x[0-9a-fA-F]{10}")
      case ARM32 => escapedRegex.replace("#addrs#", "0x[0-9a-fA-F]{5}")
      case X86_64 => escapedRegex.replace("#addrs#", "0x[0-9a-fA-F]{12}")
      case X86_64_INTEL => escapedRegex.replace("#addrs#", "0x[0-9a-fA-F]{12}")
    }
    actual.matches(expectedRegex)
  }

  private def compareStringList(
    expected: List[String],
    actual: List[String],
    matches: Boolean,
    diff: List[String],
  ): (Boolean, List[String]) =
  (expected, actual) match {
    case (Nil, Nil) => (matches, diff)
    case (e::exp, Nil) => compareStringList(exp, Nil, false, diff :+ s"- $e")
    case (Nil, a::act) =>  compareStringList(Nil, act, false, diff :+ s"+ $a")
    case (e::exp, a::act) => {
      val curMatches = matchString(e, a)
      val newDiff = if (curMatches) { diff :+ s"  $e" } else { diff :+ s"- $e" :+ s"+ $a" }
      if (e.contains("#runtime_error#") && act == List("")) (matches && curMatches, newDiff)
      else compareStringList(exp, act, matches && curMatches, newDiff)
    }
  }

  def outputEquals(actual: ByteArrayOutputStream): (Boolean, String) = {
    val str = actual.toString() 
    val lines = 
      if (str.isEmpty) List.empty[String]
      else str.split("\n", -1).toList // -1 to keep trailing empty strings
    outputEquals(lines)
  }

  def refEmulatorOutputEquals(actual: String): (Boolean, String) = {
    val lines = actual.split("\n").toList
    if (lines.size == output.size+1 && lines.last.isEmpty()) outputEquals(lines.dropRight(1))
    else if (lines.size+1 == output.size && output.last.isEmpty()) outputEquals(lines :+ "")
    else outputEquals(lines)
  }

  private def outputEquals(actualOutput: List[String]): (Boolean, String) = {
    if (undefined_behaviour) (true, "")
    else {
      val (res, dif) = compareStringList(output, actualOutput, true, List.empty)
      (res, dif.mkString("\n"))
    }
  }
}