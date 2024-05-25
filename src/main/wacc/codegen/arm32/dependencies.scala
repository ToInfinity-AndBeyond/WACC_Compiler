package wacc.codegen.arm32

import scala.collection.mutable
import scala.collection.immutable
import wacc.codegen.FileHandler.loadFile
import arm32Instr._

// This object holds the file names of the predefined functions and error messages
object Dependencies {
  // File names for predefined functions and error messages
  private final val ARRLOAD1_FILE: String = "arrLoad1.txt"
  private final val ARRLOAD4_FILE: String = "arrLoad4.txt"
  private final val ARRSTORE1_FILE: String = "arrStore1.txt"
  private final val ARRSTORE4_FILE: String = "arrStore4.txt"
  private final val ERROUTOFBOUNDS_FILE: String = "errOutOfBounds.txt"
  private final val ERRBADCHAR_FILE: String = "errBadChar.txt"
  private final val ERRDIVZERO_FILE: String = "errDivZero.txt"
  private final val ERROUTOFMEMORY_FILE: String = "errOutOfMemory.txt"
  private final val ERROVERFLOW_FILE: String = "errOverflow.txt"
  private final val ERRNULL_FILE: String = "errNull.txt"
  private final val EXIT_FILE: String = "exit.txt"
  private final val FREE_FILE: String = "free.txt"
  private final val FREEPAIR_FILE: String = "freepair.txt"
  private final val MALLOC_FILE: String = "malloc.txt"
  private final val PREFIX: String = "src/main/wacc/codegen/arm32/branchLinks/"
  private final val PRINTB_FILE: String = "printb.txt"
  private final val PRINTC_FILE: String = "printc.txt"
  private final val PRINTI_FILE: String = "printi.txt"
  private final val PRINTLN_FILE: String = "println.txt"
  private final val PRINTP_FILE: String = "printp.txt"
  private final val PRINTS_FILE: String = "prints.txt"
  private final val READC_FILE: String = "readc.txt"
  private final val READI_FILE: String = "readi.txt"

  // Returns the content of the file corresponding to the given branchLink
  def getFunction(branchLink: Dependency): String = {
    branchLink match {
      case ArrLoad1 => loadFunc(ARRLOAD1_FILE)
      case ArrLoad4 => loadFunc(ARRLOAD4_FILE)
      case ArrStore1 => loadFunc(ARRSTORE1_FILE)
      case ArrStore4 => loadFunc(ARRSTORE4_FILE)
      case ErrBadChar => loadFunc(ERRBADCHAR_FILE)
      case ErrDivZero => loadFunc(ERRDIVZERO_FILE)
      case ErrNull => loadFunc(ERRNULL_FILE)
      case ErrOutOfBounds => loadFunc(ERROUTOFBOUNDS_FILE)
      case ErrOutOfMemory => loadFunc(ERROUTOFMEMORY_FILE)
      case ErrOverflow => loadFunc(ERROVERFLOW_FILE)
      case Exit => loadFunc(EXIT_FILE)
      case Free => loadFunc(FREE_FILE)
      case FreePair => loadFunc(FREEPAIR_FILE)
      case Malloc => loadFunc(MALLOC_FILE)
      case PrintBoolean => loadFunc(PRINTB_FILE)
      case PrintChar => loadFunc(PRINTC_FILE)
      case PrintInteger => loadFunc(PRINTI_FILE)
      case Println => loadFunc(PRINTLN_FILE)
      case PrintPointer => loadFunc(PRINTP_FILE)
      case PrintString => loadFunc(PRINTS_FILE)
      case ReadChar => loadFunc(READC_FILE)
      case ReadInteger => loadFunc(READI_FILE)
    }
  }
  
  // Loads the content of the file with the given name
  private def loadFunc(file: String): String = {
    loadFile(PREFIX + file)
  }
}

// This class manages the dependencies of the program
class Dependencies() {
  // A set to hold the dependencies
  private val branchLinks: mutable.Set[Dependency] = mutable.Set()

  // Returns the current dependencies as an immutable set
  def getDependencies(): immutable.Set[Dependency] = branchLinks.toSet

  // Adds a dependency to the set
  def addDependency(predefinedLabel: Dependency): Unit = {
    // Depending on the type of predefinedLabel, add the corresponding dependencies
    predefinedLabel match {
      case ArrLoad1 | ArrLoad4 | ArrStore1 | ArrStore4 => addDependency(ErrOutOfBounds)
      case Malloc => addDependency(ErrOutOfMemory)
      case ErrDivZero | ErrNull | ErrOutOfMemory | ErrOverflow => addDependency(PrintString)
      case FreePair => addDependency(ErrNull)
      case _ => branchLinks += predefinedLabel
    }
    branchLinks += predefinedLabel
  }
  // Adds multiple dependencies to the set
  def addDependencies(predefinedLabels: immutable.Set[Dependency]): Unit = {
    predefinedLabels.foreach(addDependency)
  }
}
