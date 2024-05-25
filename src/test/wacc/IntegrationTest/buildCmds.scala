package integrationTest


import java.io.ByteArrayOutputStream
import java.io.{PrintWriter, File, FileWriter}
import java.nio.file.{StandardCopyOption, Files, Paths, Path}
import scala.concurrent.blocking
import scala.io.Source
import scala.jdk.CollectionConverters._
import wacc.codegen._
import wacc.errors.PrettyPrint

object BuildCommands {

  object Commands {
    def build_cmd(target: TargetArchitecture, executable: String, assembly: String): Seq[String] = {
      target match {
        case ARM32 => Seq("arm-linux-gnueabi-gcc", "-o", executable, "-z", "noexecstack", "-march=armv6", assembly)
        case AARCH64 => Seq("aarch64-linux-gnu-gcc", "-o", executable, "-z", "noexecstack", "-march=armv8-a", assembly)
        case _ => Seq("gcc", "-o", executable, "-z", "noexecstack", assembly) // X86_64 and X86_64_INTEL
      }
    }
    
    def emulate_cmd(target: TargetArchitecture, executable: String): Seq[String] = {
      target match {
        case ARM32 => Seq("qemu-arm", "-L", "/usr/arm-linux-gnueabi/", executable)
        case AARCH64 => Seq("qemu-aarch64", "-L", "/usr/aarch64-linux-gnu/", executable)
        case _ => Seq("./" + executable) // X86_64 and X86_64_INTEL
      }
    }

    def refCompile_cmd(srcFile: String, target: TargetArchitecture): Seq[String] = target match {
      case ARM32 => Seq("./test_cases/refCompile", srcFile, "-t", "arm32", "-a")
      case AARCH64 => Seq("./test_cases/refCompile", srcFile, "-t", "aarch64", "-a")
      case X86_64 => Seq("./test_cases/refCompile", srcFile, "-t", "x86-64", "-a")
      case X86_64_INTEL => Seq("./test_cases/refCompile", srcFile, "-t", "x86-64-intel", "-a")
    }

    def refEmulate_cmd(target: TargetArchitecture, executable: String): Seq[String] = {
      target match {
        case ARM32 => Seq("./test_cases/refEmulate", executable, "-t", "arm32")
        case AARCH64 => Seq("./test_cases/refEmulate", executable, "-t", "aarch64")
        case _ => Seq("./" + executable) // X86_64 and X86_64_INTEL
      }
    }
  }

  private def run(command: Seq[String], inp: Array[Byte], pout: ByteArrayOutputStream, err: ByteArrayOutputStream): Int = {
    val p = new java.lang.ProcessBuilder(java.util.Arrays.asList(command: _*)).start()
    blocking(p.getOutputStream.write(inp))
    p.getOutputStream.close()
    blocking(pout.write(p.getInputStream().readAllBytes()))
    blocking(err.write(p.getErrorStream().readAllBytes()))
    blocking(p.waitFor())
  }

  def runCompile(filePaths: FilePaths, exp: ExpectedOutput, result: TestResult, target: TargetArchitecture): Boolean = {
    val (errorReport, exitCode) = wacc.Main.runFile(filePaths.srcFile, filePaths.assemblyFile, target)
    if (errorReport.isDefined) result.addCompilerResult(PrettyPrint.prettyString(errorReport.get))

    if ((exp.compile_error && !exp.exitEquals(exitCode)) || (!exp.compile_error && exitCode != 0)) {
      result.addFailReason(s"Expected exit code ${exp.exit} from compiler but got exit code $exitCode")
      false
    }
    else true
  }

  def runAssembly(filePaths: FilePaths, exp: ExpectedOutput, result: TestResult, target: TargetArchitecture): Boolean = {
    // Check assembly exists
    if (!Files.exists(Paths.get(filePaths.assemblyFile))) {
      result.addFailReason(s"Assembly file ${filePaths.assemblyFile} not found")
      return false
    }
    val compileCommand = Commands.build_cmd(target, filePaths.executableFile, filePaths.assemblyFile)

    val builderStdout = new ByteArrayOutputStream()
    val builderStderr = new ByteArrayOutputStream()
    val builderExitCode = run(compileCommand, Array.emptyByteArray, builderStdout, builderStderr)
    
    result.addBuilderResult(builderStdout.toString() + "\n" + builderStderr.toString())
    if (builderExitCode != 0) {
      result.addFailReason(s"Building executable from assembly failed for ${filePaths.srcFile}")
      return false
    }

    
    val executeCommand = Commands.emulate_cmd(target, filePaths.executableFile)
    val executeStdout = new ByteArrayOutputStream()
    val executeStderr = new ByteArrayOutputStream()
    val executeExitCode = run(executeCommand, exp.input, executeStdout, executeStderr)
    result.addExecuteResult(executeStderr.toString())      
    if (!exp.exitEquals(executeExitCode)) {
      result.addFailReason(s"Expected exit code ${exp.exit} from emulator but got exit code $executeExitCode")
      return false
    }
    val (outputMatches, diff) = exp.outputEquals(executeStdout)
    if (!outputMatches) {
      result.addFailReason("Expected output and actual output do not match")
      result.addExecuteResult("diff is:\n" + diff)
      return false
    }
    if (!exp.undefined_behaviour && !executeStderr.toString().isEmpty()) {
      result.addFailReason(s"Error detected while executing ${filePaths.srcFile}")
      return false
    }
    return true
  }

  def runRefEmulator(filePaths: FilePaths, exp: ExpectedOutput, result: TestResult, target: TargetArchitecture): Boolean = {
    val executeCommand = Commands.refEmulate_cmd(target, filePaths.assemblyFile)
    val executeStdout = new ByteArrayOutputStream()
    val executeStderr = new ByteArrayOutputStream()
    run(executeCommand, exp.input, executeStdout, executeStderr)

    val executeExitCode = extractRefEmulatorExitCode(executeStdout.toString())
    if (!exp.exitEquals(executeExitCode)) {
      result.addExecuteResult(executeStdout.toString())
      result.addFailReason(s"Expected exit code ${exp.exit} from ref emulator but got exit code $executeExitCode")
      return false
    }
    val emulationOutput: Option[String] = extractEmulationOutput(executeStdout.toString())
    if (emulationOutput.isEmpty) {
      result.addFailReason(s"Emulation output not found from emulator output")
      return false
    }
    val (outputMatches, diff) = exp.refEmulatorOutputEquals(emulationOutput.get)
    if (!outputMatches) {
      result.addFailReason("Expected output and actual output do not match")
      result.addExecuteResult("diff is:\n" + diff + "\n\n" + executeStdout.toString())
      return false
    }
    result.addExecuteResult(executeStdout.toString())
    true
  }

  def runRefCompiler(filePaths: FilePaths, exp: ExpectedOutput, result: TestResult, target: TargetArchitecture): Boolean = {
    val refCompileCommand = Commands.refCompile_cmd(filePaths.srcFile, target)
    val compileStdout = new ByteArrayOutputStream()
    run(refCompileCommand, Array.emptyByteArray, compileStdout, new ByteArrayOutputStream())
    
    result.addCompilerResult(compileStdout.toString())
    // extract exit code
    val exitCode = extractRefCompilerExitCode(compileStdout.toString())
    if (exp.compile_error) {
      if (!exp.exitEquals(exitCode)) {
        result.addFailReason(s"Expected exit code ${exp.exit} from compiler but got exit code $exitCode")
        false
      }
      else true
    }
    else if (exitCode != 0) {
      result.addFailReason(s"Expected exit code 0, but compiler failed with exit code $exitCode")
      false
    }
    else {
      extractAssemblyCode(compileStdout.toString()) match {
        case None => {
          result.addFailReason(s"Assembly code not found from compiler output")
          false
        }
        case Some(str) => {
          FileHandler.appendToFile(filePaths.assemblyFile, str)
          true
        }
      }
    }
  }

  private def extractRefCompilerExitCode(compilerOutput: String): Int = {
    val ExitCodeRegex = """.*Exit code (\d+).*""".r
    compilerOutput.split("\n").collectFirst {
      case ExitCodeRegex(exitCode) => exitCode.toInt
    }.getOrElse(0)
  }

  private def extractAssemblyCode(compilerOutput: String): Option[String] = {
    val lines = compilerOutput.split("\n").toList
    val startDelimiter = "==========================================================="
    val startIndex = lines.indexOf(startDelimiter)
    val endIndex = lines.lastIndexOf(startDelimiter)
    
    if (startIndex != -1 && endIndex != -1 && startIndex < endIndex) {
      val assemblyLines = lines.slice(startIndex + 1, endIndex)
        .map(_.split("\t").drop(1).mkString("\t")) // Remove the line numbers and the first tab
      Some(assemblyLines.mkString("\n") + System.lineSeparator())
    } else {
      None
    }
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