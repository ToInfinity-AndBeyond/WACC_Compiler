package integrationTestAarch64

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.io.ByteArrayOutputStream
import java.io.{PrintWriter, File, FileWriter}
import java.nio.file.{StandardCopyOption, Files, Paths}
import scala.concurrent.blocking
import scala.io.Source
import scala.jdk.CollectionConverters._
import wacc.errors.PrettyPrint
import wacc.codegen._
import integrationTest.Main

object Aarch64TestConfig {
  // default setting is true for all options
  final val OUTPUT_TO_FILE = true          // whether result should be written to file (with .result extension)
  final val OUTPUT_TO_TERMINAL = true      // whether result should be printed to terminal
  final val OUTPUT_ONLY_FAILURE = false   // whether only failure should be printed to terminal
  final val COPY_SRC_FILES_TO_DEST = true  // whether source files should be copied to destination
}

class Aarch64MainSpec extends AnyFlatSpec {
  private final val runRefCompiler: Boolean = false
  private final val runRefEmulator: Boolean = false
  private final val skipEmulation: Boolean = false
  private final val outputDirectory = "tmp"
  private final val testDirectory = "test_cases"
  private final val customTestDirectory = "src/test/tests"
  private final val target = AARCH64 // ARM32, AARCH64, X86_64, X86_64_INTEL
  
  "Main" should "compile valid assembly for Basic Tests" in {
    runTests("valid/basic")
  }
  
  it should "compile valid assembly for Sequence Tests" in {
    runTests("valid/sequence")
  }

  it should "compile valid assembly for Input/Output Tests" in {
    runTests("valid/IO")
  }

  it should "compile valid assembly for Variable Tests" in {
    runTests("valid/variables")
  }

  it should "compile valid assembly for Expression Tests" in {
    runTests("valid/expressions")
  }

  it should "compile valid assembly for Array Tests" in {
    runTests("valid/array")
  }

  it should "compile valid assembly for Conditional Tests" in {
    runTests("valid/if")
  }

  it should "compile valid assembly for Loop Tests" in {
    runTests("valid/while")
  }

  it should "compile valid assembly for Scope Tests" in {
    runTests("valid/scope")
  }

  it should "compile valid assembly for Simple Function Tests" in {
    runTests("valid/function/simple_functions")
  }

  it should "compile valid assembly for Nested Function Tests" in {
    runTests("valid/function/nested_functions")
  }

  it should "compile valid assembly for but get errors for Runtime Error Tests" ignore {
    runTests("valid/runtimeErr")
  }

  it should "compile valid assembly for Heap Tests" in {
    runTests("valid/pairs")
  }

  it should "compile valid assembly for advanced tests without emulating" in {
    runTests("valid/advanced", false)
  }

  it should "correctly return syntax errors" ignore {
    runTests("invalid/syntaxErr")
  }

  it should "correctly return semantic errors" ignore {
    runTests("invalid/semanticErr")
  }

  it should "run custom syntax error tests" in {
    runCustomTests("syntaxError")
  }

  it should "run custom semantic error tests" in {
    runCustomTests("semanticError")
  }

  it should "compile custom valid tests to assembly" in {
    runCustomTests("valid")
  }

  it should "compile custom other tests to assembly" in {
    runCustomTests("other", true, ignoreFiles = List("sample/runtimeErr.wacc"))
  }

  it should "compile custom read pair test to assembly" in {
    runCustomTests("valid/customBackend")
  }

  it should "compile custom optimization test" in {
    runCustomTests("valid/optimizationTest")
  }

  it should "compile valid assembly for Overload Function Tests" in {
    runCustomTests("functionOverloading")
  }


  private def runCustomTests(dir: String, emulate: Boolean = true, ignoreFiles: List[String] = List()) = {
    if (skipEmulation) runDir(customTestDirectory + "/" + dir, outputDirectory + "/custom/" + dir, false, ignoreFiles)
    else runDir(customTestDirectory + "/" + dir, outputDirectory + "/custom/" + dir, emulate, ignoreFiles)
  }

  private def runTests(dir: String, emulate: Boolean = true, ignoreFiles: List[String] = List()) = {
    if (skipEmulation) runDir(testDirectory + "/" + dir, outputDirectory + "/" + dir, false)
    else runDir(testDirectory + "/" + dir, outputDirectory + "/" + dir, emulate, ignoreFiles)
  }

  private def runDir(srcDir: String, destDir: String, emulate: Boolean, ignoreFiles: List[String] = List()) = {
    val result: Boolean = Main.runDirectory(srcDir, destDir, target, emulate, runRefCompiler, runRefEmulator, ignoreFiles)
    result shouldBe true
  }
}
