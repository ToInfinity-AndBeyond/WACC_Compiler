package wacc.optimization
import wacc.semantic.SymbolTree
import wacc.ast.Types._

import scala.collection.mutable

private class ConstantMap(constantMap: mutable.Map[String, Option[Int]], 
                          parentConstantState: Option[ConstantMap] = None) {
  private val identDeclared: mutable.Map[String, Boolean] = mutable.Map()

  def printConstantMap(): Unit = {
    println(constantMap)
    parentConstantState match {
      case Some(parent) => parent.printConstantMap()
      case None => 
    }
  }

  def getParentConstantMap(): Option[ConstantMap] = parentConstantState

  def hasLocalIdent(identName: String): Boolean = constantMap.contains(identName)
  
  // constantMap contains a mapping from the IdentName(String) to its literal value(Option[Atom])
  // None means that the variable is not a constant
  // An ident always exit in the constantMap, even if it is not a constant
  def setIdentAsConstant(identName: String, value: Option[Int]): Unit = {
    constantMap += (identName -> value)
  }

  def setIdentDeclared(identName: String, declared: Boolean): Unit = {
    identDeclared += (identName -> declared)
  }

  // Iterate to the first parentScope that has the variable
  def setIdentAsConstantOverall(identName: String, value: Option[Int]): Unit = {
    if (constantMap.contains(identName)) 
      constantMap += (identName -> value)
    else {
      parentConstantState match {
        case None => throw new RuntimeException(s"$identName not found even in the parent scope")
        case Some(parent) => parent.setIdentAsConstantOverall(identName, value)
      }
    }
  }

  def getIdentValue(identName: String): Option[Int] = {
    constantMap.get(identName) match {
      case None => {
        parentConstantState match {
          case None => throw new RuntimeException(s"Identifier '$identName' not found in constantMap")
          case Some(parent) => parent.getIdentValue(identName)
        }
      }
      case Some(value) => value
    }
  }

  def identExist(identName: String): Boolean = {
    identDeclared.get(identName) match {
      case None => false
      case _ => true
    }
  }

  def isIdentDeclared(identName: String): Boolean = {
    identDeclared.get(identName) match {
      case None => {
        parentConstantState match {
          case None => throw new RuntimeException(s"$identName not found in identDeclared Map!")
          case Some(parent) => parent.isIdentDeclared(identName)
        }
      }
      case Some(value) => value
    }
  }
}

private class ConstantState(symbolTree: SymbolTree, 
                            constantMap: ConstantMap) {
                            // constantMap: mutable.Map[String, Option[Int]], 
                            // parentConstantState: Option[ConstantState] = None) {

  private var _curSymbolTree: SymbolTree = symbolTree
  def curSymbolTree: SymbolTree = _curSymbolTree
  private val curLoc: mutable.Stack[Int] = mutable.Stack(0)

  private var _curConstantMap: ConstantMap = constantMap
  def curConstantMap: ConstantMap = _curConstantMap

  private var tempVarCount = 0

  def printConstantMap() = {
    println("Constant Map: ")
    constantMap.printConstantMap()
  }

  def getNextTempVar(): String = {
    val nextTempVar = s"($tempVarCount)_tempVar"
    tempVarCount += 1
    nextTempVar
  }

  def hasLocalIdent(identName: String): Boolean = {
    curConstantMap.hasLocalIdent(identName)
  }
  
  def enterScope() = {
    // println("enter scope")
    // println(curLoc)
    // println(curSymbolTree.currentScope)

    val nextChildIndex = curLoc.pop()
    if (nextChildIndex < curSymbolTree.childScopes.length) {
      _curConstantMap = new ConstantMap(mutable.Map(), Some(curConstantMap))
      _curSymbolTree = curSymbolTree.childScopes(nextChildIndex)
      curLoc.push(nextChildIndex + 1)
      curLoc.push(0)
    }
  }

  // Moves up one level in the scope hierarchy
  def exitScope() = {
    // println("exit scope")
    // println(curLoc)
    curLoc.pop()

    curConstantMap.getParentConstantMap() match {
      case None => //throw new RuntimeException("Parent not found for the constant Map!")
      case Some(parent) => {
        _curConstantMap = parent
        _curSymbolTree = curSymbolTree.parentScope.get
      }
    }
  }

  def getIdentType(identName: String): Type = curSymbolTree.lookUp(identName)

  def setIdentAsConstant(identName: String, value: Option[Int]): Unit = {
    curConstantMap.setIdentAsConstant(identName, value)
  }

  def getIdentValue(identName: String): Option[Int] = {
    curConstantMap.getIdentValue(identName)
  }

  def setIdentDeclared(identName: String, declared: Boolean): Unit = {
    curConstantMap.setIdentDeclared(identName, declared)
  }

  def isIdentDeclared(identName: String): Boolean = {
    curConstantMap.isIdentDeclared(identName)
  }

  def setIdentAsConstantOverall(identName: String, value: Option[Int]): Unit = {
    curConstantMap.setIdentAsConstantOverall(identName, value)
  }

  // // constantMap contains a mapping from the IdentName(String) to its literal value(Option[Atom])
  // // None means that the variable is not a constant
  // // An ident always exit in the constantMap, even if it is not a constant
  // def setIdentAsConstant(identName: String, value: Option[Int]): Unit = {
  //   constantMap += (identName -> value)
  // }

  // def getIdentValue(identName: String): Option[Int] = {
  //   constantMap.get(identName) match {
  //     case None => throw new RuntimeException(s"Identifier '$identName' not found in constantMap")
  //     case Some(value) => value
  //   }
  // }
}
