package wacc.syntax

import parsley.{Parsley, Result}
import parsley.expr._
import parsley.Parsley._
import parsley.combinator._
import parsley.errors.combinator._
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors._
import parsley.errors.patterns.PreventativeErrors

import lexer.implicits.implicitSymbol
import lexer.{fully}
import wacc.ast.ast._
import wacc.ast.Types._
import java.io.File
import wacc.errors.WaccError._

object parser {
  implicit val errBuilder: ErrorBuilder[WaccError] = new WaccErrorBuilder with TillNextWhitespace {
    def trimToParserDemand: Boolean = false
  }

  // Public methods to parse input from a string or file into a Program AST node.
  def parse(file: String) = program.parse(file)
  def parseFile(file: File) = program.parseFile(file)

  private lazy val intliter : Parsley[IntLiter] = IntLiter(lexer.integer)
  private lazy val boolliter : Parsley[BoolLiter] = BoolLiter("true" #> true | "false" #> false)
  private lazy val charliter : Parsley[CharLiter] = CharLiter(lexer.characterLiteral)
  private lazy val strliter : Parsley[StrLiter] = StrLiter(lexer.stringLiteral)
  private lazy val pairliter : Parsley[Expr] = (PairLiter <# "null")
  private lazy val ident = _noPointerCheck ~> Ident(lexer.ident)
  /* 
    Expression Parser using a precedence climbing method for binary and unary operations.
      <expr> ::= ⟨unary-oper⟩⟨expr⟩
              | ⟨expr⟩⟨binary-oper⟩⟨expr⟩ | ⟨atom⟩
   */ 

  private lazy val expr : Parsley[Expr] = precedence(
    Atoms(atom) :+
    SOps(Prefix)(NOT <# "!", NEG <# lexer.NEGATE, LEN <# "len", ORD <# "ord", CHR <# "chr"):+
    SOps(InfixL)(MUL <# "*", MOD <# "%", DIV <# "/"):+
    SOps(InfixL)(ADD <# "+", SUB <# "-"):+
    SOps(InfixN)(GT <# ">", GTE <# ">=", LT <# "<", LTE <# "<="):+
    SOps(InfixN)(EQ <# "==", NEQ <# "!="):+
    SOps(InfixR)(AND <# "&&"):+
    SOps(InfixR)(OR <# "||")
  )

  /* 
    Atom Parser
      <atom> ::= ⟨int-liter⟩
              | ⟨bool-liter⟩
              | ⟨char-liter⟩
              | ⟨str-liter⟩
              | ⟨pair-liter⟩
              | ⟨ident⟩
              | ⟨array-elem⟩ | ‘(’ ⟨expr⟩ ‘)’
   */  
  private lazy val atom =
    intliter | boolliter | charliter | strliter | pairliter | 
    Bracket("(" ~> expr <~")") | arrayelem | ident 


  /*
    Array Element Parser
      <array-elem> ::= ⟨ident ⟩ (‘[’ ⟨expr⟩ ‘]’)+ 
   */
  private lazy val arrayelem = atomic(ArrayElem(ident, some("[" ~> expr <~ "]")))

  /* 
    Type Parsers
      <type> ::= ⟨base-type⟩ | ⟨array-type⟩ | ⟨pair-type⟩
   */
  private lazy val parseType : Parsley[Type] =  atomic(arrayType) <|>
    atomic(pairType) <|>
    baseType

  /* 
    Base Type Parsers
      <base-type> ::= ‘int’ | ‘bool’ | ‘char’ | ‘string’
   */
  private lazy val baseType: Parsley[BaseType] = 
    (("int" #> IntType) <|> 
    ("bool" #> BoolType) <|> 
    ("char" #> CharType) <|> 
    ("string" #> StringType))
    

  /* 
    Array Type Parser
      <array-type> ::= ⟨type⟩ ‘[’ ‘]’
   */
  private lazy val arrayType: Parsley[ArrayType] =
    chain.postfix1(baseType <|> pairType)(ArrayType <# ("[" ~> "]"))

  /* 
    Pair Type Parser
      <pair-type> ::= ‘pair’ ‘(’ ⟨pair-elem-type⟩ ‘,’ ⟨pair-elem-type⟩ ‘)’
   */
  private lazy val pairType: Parsley[PairType] =
    PairType(
      "pair" ~> "(" ~> pairElemType,
      "," ~> pairElemType <~ ")"
    )
    

  /*
    Pair Element Type Parser
      <pair-elem-type> ::= ⟨base-type⟩ | ⟨array-type⟩ | ‘pair’
   */
  private lazy val pairElemType: Parsley[PairElemType] =
    atomic(arrayType) <|> 
    atomic(baseType) <|> 
    ("pair" #> ErasedPairType)

  /*
    Program Parser_noNamedPairCheck ~> 
      <program> ::= ‘begin’ ⟨func⟩* ⟨stmt⟩ ‘end’ 
   */
  private lazy val program : Parsley[Program] =
    fully("begin" ~> Program(many(func), sepBy1(stmt, ";")).explain("Body is missing") <~ "end")

  /* 
    Function Parser
      <func> ::= ⟨type⟩ ⟨ident⟩ ‘(’ ⟨param-list⟩? ‘)’ ‘is’ ⟨stmt⟩ ‘end’
   */
  private lazy val func =
    atomic(lookAhead(parseType ~> ident ~> "("))~>Func(
      parseType,
      ident,
      "(" ~> paramlist <~ ")",
      "is" ~> sepBy1(stmt, ";").filterOut(filterFuncWithNoReturn)
      .explain("All exit paths of function must exit or return") <~ "end"
    )
  
  /* 
    Helper for filtering out functions without a return or exit statement, 
    ensuring all paths return or exit.
   */
  private val filterFuncWithNoReturn = new PartialFunction[List[Stmt], String] {
    def apply(v1: List[Stmt]): String = ""
    def isDefinedAt(stmts: List[Stmt]): Boolean = {
      stmts.last match {
        case IfStmt(expr, thenStmt, elseStmt) => isDefinedAt(thenStmt) || isDefinedAt(elseStmt)
        case WhileStmt(expr, doStmt)          => isDefinedAt(doStmt)
        case Scope(stmt)                      => isDefinedAt(stmt)
        case Return(_)                        => false
        case Exit(_)                          => false
        case _                                => true
      }
    }
  }

  /* 
    Parameter List Parser
      <param-list> ::= ⟨param⟩ ( ‘,’ ⟨param⟩ )*
   */
  private lazy val paramlist = sepBy(param, ",")

  /* 
    Parameter Parser
      <param> ::= ⟨type⟩ ⟨ident⟩
   */
  private lazy val param = Param(parseType, ident)

  /* 
    Statement Parsers
      <stmt> ::= ‘skip’
              | ⟨type⟩ ⟨ident⟩ ‘=’ ⟨rvalue⟩
              | ⟨lvalue⟩ ‘=’ ⟨rvalue⟩
              | ‘read’ ⟨lvalue⟩
              | ‘free’ ⟨expr⟩
              | ‘return’ ⟨expr⟩
              | ‘exit’ ⟨expr⟩
              | ‘print’ ⟨expr⟩
              | ‘println’ ⟨expr⟩
              | ‘if’ ⟨expr⟩ ‘then’ ⟨stmt⟩ ‘else’ ⟨stmt⟩ ‘fi’
              | ‘while’ ⟨expr⟩ ‘do’ ⟨stmt⟩ ‘done’
              | ‘begin’ ⟨stmt⟩ ‘end’
              | ⟨stmt⟩ ‘;’ ⟨stmt⟩
    */
  private lazy val stmt : Parsley[Stmt] = _unCommentedCheck ~> (
    Skip <# "skip"
    | Declare(parseType, ident, "=" ~>rvalue)
    | Assign(lvalue, "=" ~> rvalue)
    | Read("read" ~> lvalue)
    | Free("free" ~> expr)
    | Return("return" ~> expr)
    | Exit("exit" ~> expr)
    | Print("print" ~> expr.explain("Print and Println can only print expressions"))
    | Println("println" ~> expr.explain("Print and Println can only print expressions"))
    | IfStmt(
      "if" ~> expr,
      "then".explain("if statements must have a \"then\" clause") 
        ~> sepBy1(stmt, ";") <~ _noDoubleSemicolonCheck,
      "else".explain("if statements must have an \"else\" clause") 
        ~> sepBy1(stmt, ";") <~ _noDoubleSemicolonCheck 
        <~ "fi".explain("if statements must end with \"fi\"")
    )
    | WhileStmt(
      "while" ~> expr,
      "do".explain("while loops must have a \"do\" clause") 
        ~> sepBy1(stmt, ";") <~ _noDoubleSemicolonCheck 
        <~ "done".explain("while loops must have a \"done\" clause") 
    )
    | Scope("begin" ~> sepBy1(stmt, ";") <~ "end")) <~ _noDoubleSemicolonCheck

  /* 
    LValue Parsers
      <lvalue> ::= ⟨ident⟩ | ⟨array-elem⟩ | ⟨pair-elem⟩
   */

  private lazy val lvalue : Parsley[lValue] = (
    atomic(pairelem) | atomic(arrayelem)) | atomic(ident)
  
  /* 
    RValue Parsers
      ⟨rvalue⟩ ::= ⟨expr⟩
                | ⟨array-liter ⟩
                | ‘newpair’ ‘(’ ⟨expr ⟩ ‘,’ ⟨expr ⟩ ‘)’ 
                | ⟨pair-elem⟩
                | ‘call’ ⟨ident⟩ ‘(’ ⟨arg-list⟩? ‘)’
   */ 
  private lazy val rvalue : Parsley[rValue] = (
    expr | arrayliter | NewPair("newpair" ~> "(" ~> expr <~ ",", expr <~ ")") 
      | pairelem | Call("call" ~> ident, "(" ~> arglist <~ ")"))

  /* 
    Argument List Parsers
      <arg-list> ::= ⟨expr⟩ (‘,’ ⟨expr⟩ )*
   */
  private lazy val arglist = ArgList(sepBy(expr, ",")).explain("E 14")

  /*
    Pair Element Parsers 
      <pair-elem> ::= ⟨expr⟩ (‘,’ ⟨expr⟩ )*
   */
  private lazy val pairelem = 
    Fst("fst" ~> lvalue.explain("'fst' or 'snd' requires pair (or nested pair) as argument")) | 
    Snd("snd" ~> lvalue.explain("'fst' or 'snd' requires pair (or nested pair) as argument"))
                              

  /*
    Array Literal Parser
      <array-liter> ::= ‘[’ ( ⟨expr⟩ (‘,’ ⟨expr⟩)* )? ‘]’
   */
  private lazy val arrayliter = ArrayLiter("[" ~> sepBy(expr, ",") <~ "]") <~ _noConcatCheck

  private lazy val _noDoubleSemicolonCheck = atomic(";" <~> ";").preventativeExplain(
    reason = "Semicolons are used to separate statements. Redundant semicolons are not allowed in WACC"
  )

  private lazy val _noConcatCheck = atomic("++").preventativeExplain(
    reason = "List concatenation is not allowed in wacc"
  )

  private lazy val _unCommentedCheck = atomic(ident <~> ident <~> ident).preventativeExplain(
    reason = "Perhaps you forgot to comment out some code?"
  )

  private lazy val _noPointerCheck = (
    "*" <~> Ident(lexer.ident) | "&" <~> Ident(lexer.ident)).preventativeExplain(
    reason = "Pointers are not allowed in WACC"
  )
}
