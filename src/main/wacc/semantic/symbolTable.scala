package wacc.semantic

import scala.collection.mutable
import scala.collection.immutable
import wacc.ast._
import Types._
import wacc.errors.WaccError._
import mutable.ListBuffer
import wacc.ir.statementTranslator.typeToInt

// Immutable version of SymbolTable that should be used for IR builder
class SymbolTree(_parentScope: =>Option[SymbolTree], _currentScope: immutable.Map[String, Type], _childScopes: List[SymbolTree]) {
  lazy val parentScope = _parentScope
  val currentScope = _currentScope
  val childScopes = _childScopes

  override def toString(): String = s"$currentScope contains $childScopes"

  // Lookup a variable or function in the current scope or parent scopes
  def lookUp(name: String): Type = {
    currentScope.get(name) match {
      // If found in the current scope, return its type
      case Some(t) => t
      // If not found, recurse up to the parent scope
      case None => parentScope match {
        case Some(parent) => parent.lookUp(name) 
        // If no parent, return an error
        case None => throw new Exception(s"Variable ${name} not declared in this scope")
      }
    }
  }
}

object SymbolTree {
  // Translate a SymbolTable into an immutable SymbolTree
  def freeze(table: SymbolTable, parent: =>Option[SymbolTree] = None): SymbolTree = {
    lazy val tree: SymbolTree = new SymbolTree(parent, table.getCurrentScope(), table.getChildScopes().toList.map(freeze(_, Some(tree))))
    tree
  }
}

// Define the SymbolTable class to manage variable and function symbols within scopes
class SymbolTable(val parentScope: Option[SymbolTable]) {
  
  // Add the current SymbolTable as a child scope of the parent scope during construction
  parentScope match {
    case Some(ps) => ps.addChildScope(this)
    case None =>
  }
  // A map to hold the current scope's symbols with their names as keys and their types as values
  private val currentScope: mutable.Map[String, Type] = mutable.Map()
  // Mutable list of child scopes of the current scope
  private val childScopes: ListBuffer[SymbolTable] = new ListBuffer[SymbolTable]

  // Generates a unique identifier for a function by combining the function name and the types of its parameters.
  def generateUniqueIdent(name: String, params: List[Type]): String = {
    val paramString = params.map(param => typeToInt(param)).mkString("")
    val uniqueFuncName = s"${paramString}_${name}"
    uniqueFuncName
  }



  // Insert a child scope in the current scope's childScope (mutable list)
  def addChildScope(childScope: SymbolTable) = childScopes.addOne(childScope)

  // Get child scopes of the current symbol table
  def getChildScopes(): ListBuffer[SymbolTable] = childScopes
  
  // Get current scope (hash map of variables)
  def getCurrentScope(): immutable.Map[String, Type] = immutable.Map(currentScope.toSeq: _*)
  
  // Insert a variable into the current scope
  def insert(name: String, pos: (Int, Int), literalType: Type): Either[List[WaccError], String] = {
    if (currentScope.contains(name)) {
      // Return an error if the variable is already defined in this scope
      Left(List(ScopeError(None, None, None, Some(s"Variable '$name' is already defined in this scope"), pos)))
    } else {
      // Add the variable to the current scope and return its name
      currentScope += (name -> literalType)
      Right(name)
    }
  }

  // Insert a function into the current scope with a unique identifier
  def insertFunc(name: String, pos: (Int, Int), literalType: FuncType): Either[List[WaccError], String] = {
    val uniqueFuncName = generateUniqueIdent(name, literalType.args) 
    if (currentScope.contains(uniqueFuncName)) {
      // Return an error if the function is already defined in this scope
      Left(List(ScopeError(Some("Function declaration"), None, None, 
                           Some(s"Function '$name' is already defined in this scope"), pos)))
    } else {
      // Add the function to the current scope with its unique identifier and return the identifier
      currentScope += (uniqueFuncName -> literalType)
      Right(uniqueFuncName)
    }
  }

  // Lookup a variable or function in the current scope or parent scopes
  def lookUp(name: String, pos: (Int, Int)): Either[List[WaccError], Type] = {
    currentScope.get(name) match {
      // If found in the current scope, return its type
      case Some(t) => Right(t)
      // If not found, recurse up to the parent scope
      case None => parentScope match {
        case Some(parent) => parent.lookUp(name, pos) 
        // If no parent, return an error
        case None => Left(List(ScopeError(None, Some(s"${name}"), None, 
                          Some(s"Variable ${name} not declared in this scope"), pos))) 
      }
    }
  }

  // Lookup a function in the current scope or parent scopes using its unique identifier
  def lookUpFunc(name: String, args: List[Type], pos: (Int, Int)): Either[List[WaccError], Type] = {
    // Generate the unique identifier for the function
    val fName = generateUniqueIdent(name, args) 
    // Perform the lookup with the unique identifier
    this.lookUp(fName, pos) match {
      case Left(err) => Left(List(ScopeError(None, Some(s"${name}"), None, 
                          Some(s"Variable ${name} not declared in this scope"), pos))) 
      case Right(t) => Right(t)
    }
  }
}
