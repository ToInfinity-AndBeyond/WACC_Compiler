package wacc.codegen

import wacc.errors.WaccError._
import wacc.ir.IR
import wacc.codegen.arm32.arm32StringBuilder
import wacc.codegen.aarch64.aarch64StringBuilder

object codeGenerator {
  // Generates code for the target architecture, specifically ARM32.
  def generateCode(ir: IR, destFile: String, target: TargetArchitecture): Option[WaccErrorReport] = {
    try {
      target match {
        case ARM32 => arm32StringBuilder.generate(ir, destFile)
        case AARCH64 => aarch64StringBuilder.generate(ir, destFile)
        case _ => throw new UnsupportedOperationException("Unsupported target architecture")
      }
      None
    } catch {
      case e: Exception => {
        e.printStackTrace()
        Some(new UnexpectedError("Code generation failed" + e.getMessage))
    
      }
    }
  }
}
