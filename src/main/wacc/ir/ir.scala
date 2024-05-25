package wacc.ir

import scala.collection.mutable
import threeAddrCode.{ThreeAddrInstr, TStringLabel}
import wacc.ir.threeAddrCode._
import wacc.semantic.SymbolTree

class IR (val strings: List[TStringLabel], val funcs: List[ThreeAddrFunc])

class ThreeAddrFunc(val name: String, val instrs: List[ThreeAddrInstr], val argNum: Int, val usedLocs: Int)

// class used to store the state during IR generation
// symboltree is reused from the semantic analysis, and a similar locTable is created to store the locations of variables
// stringTable is used to store the strings in the program
class State(private val symbolTree: SymbolTree) {
  private val locTable: LocTable = new LocTable(None, mutable.Map(), mutable.Map(), List())
  private var labelCount: Int = 0

  private var _curLocTable: LocTable = locTable
  def curLocTable: LocTable = _curLocTable
  
  private var _curSymbolTree: SymbolTree = symbolTree
  def curSymbolTree: SymbolTree = _curSymbolTree

  val stringTable: StringTable = new StringTable()

  private val curLoc: mutable.Stack[Int] = mutable.Stack(0)
  def locsUsed: Int = curLocTable.maxLocCount

  // Moves down one level in the scope hierarchy
  def moveScopeDown() = {
    val nextChildIndex = curLoc.pop()
    if (nextChildIndex < curSymbolTree.childScopes.length) {
      // Update the current location and symbol tree
      _curLocTable = new LocTable(Some(curLocTable), mutable.Map(), mutable.Map(), List())
      _curSymbolTree = curSymbolTree.childScopes(nextChildIndex)
      // Increment the index in the visited indices stack
      curLoc.push(nextChildIndex + 1)
      curLoc.push(0)
    }
  }
  // Moves up one level in the scope hierarchy
  def moveScopeUp() = {
    curLoc.pop()
    val maxLocCount = curLocTable.maxLocCount
    curLocTable.parentScope match {
      case Some(parent) =>
        _curLocTable = parent
        curLocTable.maxLocCount = maxLocCount.max(curLocTable.maxLocCount)
        _curSymbolTree = curSymbolTree.parentScope.get
      case None => // If already at the root, do nothing or handle as needed
    }
  }

  // Enters a new local scope
  def enterLocScope() = {
    _curLocTable = new LocTable(Some(_curLocTable), mutable.Map(), mutable.Map(), List())
  }

  // Exits the current local scope
  def exitLocScope() = {
    val maxLocCount = curLocTable.maxLocCount
    _curLocTable = curLocTable.parentScope.get
    curLocTable.maxLocCount = maxLocCount.max(curLocTable.maxLocCount)
  }

  // Generates a new branch label
  def getNextTBranchLabel(): TBranchLabel = {
    val label = new TBranchLabel(labelCount)
    labelCount += 1
    label
  }
}

// Represents a table of locations in the current scope
class LocTable(_parentScope: Option[LocTable], 
  val currentScope: mutable.Map[String, TLoc], 
  val identConstantVals: mutable.Map[String, Option[TImm]],
  val childScopes: List[LocTable]
) {
  val parentScope: Option[LocTable] = _parentScope
  private var locCount: Int = parentScope match {
    case Some(parent) => parent.locCount
    case None => 0
  }
  var maxLocCount: Int = locCount
  
  // Generates a new register location
  def getNextTRegLoc(): TLoc = {
    val loc = new TRegLoc(locCount)
    locCount += 1
    maxLocCount = maxLocCount.max(locCount)
    loc
  }
  // Adds an identifier to the current scope
  def addIdent(name: String, loc: TLoc): Unit = {
    currentScope += (name -> loc)
  }

  // Looks up an identifier in the current and parent scopes
  def lookUpIdent(name: String): TLoc = {
    currentScope.get(name) match {
      case Some(loc) => loc
      case None => parentScope match {
        case Some(parent) => parent.lookUpIdent(name)
        case None => throw new RuntimeException(s"Identifier $name not found in LocTable")
      }
    }
  }

  // Set Ident as a constant
  // If it is None, it is not a constant.
  // If it is an arrayElem, name of "a[index]" will be constant
  // If it is a pairElem, name of "a[fst]" or "a[snd]" will be constant
  def setIdentConstant(name: String, constantValue: Option[TImm]): Unit = {
    identConstantVals += (name -> constantValue)
  }

  // getIdentConstant assumes that any ident not in the identConstVals as non-constant ident
  // lookupIdent will fail if the ident actually does not exist.
  def getIdentConstant(name: String): Option[TImm] = {
    identConstantVals.get(name) match {
      case Some(constantVal) => constantVal
      case None => parentScope match {
        case Some(parent) => parent.getIdentConstant(name)
        case None => None
      }
    }
  }


}

// Represents a table of strings
class StringTable {
  private val stringTable: mutable.Map[String, TStringLabel] = mutable.Map()

  // Adds a string to the table, or returns the existing label if it already exists
  def addString(str: String): TStringLabel = {
    if (stringTable.contains(str)) stringTable(str)
    else {
      val strCount = stringTable.size
      val label = new TStringLabel(strCount, str)
      stringTable += (str -> label)
      label
    }
  }

  // Returns a list of all string labels in the table
  def toStringList(): List[TStringLabel] = {
    stringTable.values.toList
  }
}