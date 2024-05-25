package backendUnitTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.ir._
import wacc.codegen.ARM32

import java.io.{PrintWriter, File, FileWriter}

class Arm32CodeGenUnitTest extends AnyFlatSpec with Matchers {
  private final val outputDirectory = "tmp/Arm32CodeGenUnitTest"
  private final val runRefEmulator = false
  private final val target = ARM32

  // "Arm32 Code Generation" should "generate valid assembly" ignore {
  //   val filename = outputDirectory + "/assembly.s"
  //   val ir: IR = new IR(List(), List(new ThreeAddrFunc("main",List(), 0, 0)))
  //   val input = ""
  //   val expectedExitCode = 0
  //   val expectedOutput = "Hello, World!\n"
    
  //   buildAssemblyAndEmulate(filename, ir, input, expectedExitCode, expectedOutput)
  // }

  private def buildAssemblyAndEmulate(
    filename: String,
    ir: IR,
    input: String,
    expectedExitCode: Int,
    expectedOutput: String
  ): Unit = {
    wacc.codegen.codeGenerator.generateCode(ir, filename, target) match {
      case Some(err) => fail("failed to generate assembly: " + err)
      case None => {
        val (result, exitCode, msg) = AssemblyEmulation.runAssembly(filename, input, target, runRefEmulator)
        if (!result) fail("failed to emulate assembly: " + msg)
        exitCode shouldEqual expectedExitCode
        msg shouldEqual expectedOutput
      }
    }
  }

  private def createFile(filename: String): Boolean = {
    val file = new File(filename)
    try {
      file.getParentFile.mkdirs() // Create the directory if it doesn't exist
      file.exists() || file.createNewFile()
    } catch {
      case _: Throwable => false
    }
  }

  def writeToFile(filename: String, content: String): Boolean = {
    println(content)
    println(filename)
    val fileExists = createFile(filename)
    if (!fileExists) false
    else {
      val writer = new PrintWriter(new File(filename))
      try {
        writer.write(content)
        true
      } catch {
        case _: Throwable => false
      } finally {
        writer.close()
      }
    }
  }
}