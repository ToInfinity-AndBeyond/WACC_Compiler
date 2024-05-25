package wacc.codegen

sealed trait TargetArchitecture
case object ARM32 extends TargetArchitecture
case object AARCH64 extends TargetArchitecture
case object X86_64 extends TargetArchitecture
case object X86_64_INTEL extends TargetArchitecture