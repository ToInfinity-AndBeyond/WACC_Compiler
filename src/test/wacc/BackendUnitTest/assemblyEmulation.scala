package backendUnitTest

import scala.concurrent.blocking
import java.io.ByteArrayOutputStream
import java.io.{PrintWriter, File, FileWriter}
import java.nio.file.{StandardCopyOption, Files, Paths}
import wacc.codegen.TargetArchitecture

import integrationTest.BuildCommands.Commands

object AssemblyEmulation {
  private final val FAIL: Int = -1

  // Returns success of emulation, exit code and output
  def runAssembly(srcFile: String, input: String, target: TargetArchitecture, refEmulator: Boolean = false): (Boolean, Int, String) = {
    if (refEmulator) {
      runRefEmulator(srcFile, input, target) 
    } else {
      runAssembly(srcFile, input, target)
    }
  }

  private def runAssembly(srcFile: String, input: String, target: TargetArchitecture): (Boolean, Int, String) = {
    // Check assembly exists
    if (!Files.exists(Paths.get(srcFile))) {
      return (false, FAIL, "Assembly file does not exist")
    }
    val executableFile: String = srcFile.replace(".s", "")
    val compileCommand = Commands.build_cmd(target, executableFile, srcFile)

    val builderStdout = new ByteArrayOutputStream()
    val builderStderr = new ByteArrayOutputStream()
    val builderExitCode = run(compileCommand, Array.emptyByteArray, builderStdout, builderStderr)
    
    if (builderExitCode != 0) {
      return (false, FAIL, "Building executable from assembly failed")
    }

    val executeCommand = Commands.emulate_cmd(target, executableFile)
    val executeStdout = new ByteArrayOutputStream()
    val executeStderr = new ByteArrayOutputStream()
    val executeExitCode = run(executeCommand, input.getBytes(), executeStdout, executeStderr)

    if (!executeStderr.toString().isEmpty()) {
      val errMsg = s"Error detected while executing ${srcFile}" + executeStderr.toString()  
      return (false, executeExitCode, errMsg)
    }
    return (true, executeExitCode, executeStdout.toString())
  }

  private def runRefEmulator(srcFile: String, input: String, target: TargetArchitecture): (Boolean, Int, String) = {
    val executeCommand = Commands.refEmulate_cmd(target, srcFile)
    val executeStdout = new ByteArrayOutputStream()
    val executeStderr = new ByteArrayOutputStream()
    run(executeCommand, input.getBytes(), executeStdout, executeStderr)

    val executeExitCode = extractRefEmulatorExitCode(executeStdout.toString())

    val emulationOutput: Option[String] = extractEmulationOutput(executeStdout.toString())
    if (emulationOutput.isEmpty) {
      return (false, FAIL, "Emulation output not found from emulator output")
    }
    return (true, executeExitCode, emulationOutput.get)
  }

  private def run(command: Seq[String], inp: Array[Byte], pout: ByteArrayOutputStream, err: ByteArrayOutputStream): Int = {
    val p = new java.lang.ProcessBuilder(java.util.Arrays.asList(command: _*)).start()
    blocking(p.getOutputStream.write(inp))
    p.getOutputStream.close()
    blocking(pout.write(p.getInputStream().readAllBytes()))
    blocking(err.write(p.getErrorStream().readAllBytes()))
    blocking(p.waitFor())
  }

   private def extractRefEmulatorExitCode(emulatorOutput: String): Int = {
    val ExitCodeRegex = """.*The exit code is: (\d+).*""".r
    emulatorOutput.split("\n").collectFirst {
      case ExitCodeRegex(exitCode) => exitCode.toInt
    }.getOrElse(0)
  }

  private def extractEmulationOutput(emulatorOutput: String): Option[String] = {
    val startDelimiter = "-- Emulation Output:\n"
    val endDelimiter = "\n---------------------------------------------------------------"
    val startIndex = emulatorOutput.indexOf(startDelimiter)
    if (startIndex != -1) {
      val endIndex = emulatorOutput.indexOf(endDelimiter, startIndex + startDelimiter.length)
      if (endIndex != -1) {
        Some(emulatorOutput.substring(startIndex + startDelimiter.length, endIndex))
      } else {
        None // End delimiter not found
      }
    } else {
      None // Start delimiter not found
    }
  }
}