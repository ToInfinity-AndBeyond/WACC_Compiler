package wacc.syntax

import parsley.Parsley
import parsley.Parsley._
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import parsley.character.{noneOf, digit, string, char}

object lexer {
  // Customizes the lexical description to define rules specific to WACC language.
  private val desc = LexicalDesc.plain.copy(
    // Specifies reserved keywords and operators unique to WACC language.
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set(
        "begin", "end", "is", "skip",
        "read", "free", "return", "exit",
        "print", "println", "if", "then",
        "else", "fi", "while", "do",
        "done", "newpair", "call", "fst",
        "snd", "int", "bool", "char", "string",
        "pair", "true", "false", "null",
        "len", "ord", "chr"
      ),
      hardOperators = Set(
        "!", "-",
        "*", "%", "/", "+",
        ">", ">=", "<", "<=", "==",
        "!=", "&&", "||"
      ),
    ),
    // Sets rules for what constitutes a valid identifier in the WACC language.
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(ch => ch.isLetter || ch == '_'),
      identifierLetter = predicate.Basic(ch => ch.isLetterOrDigit || ch == '_'),
    ),
    // Defines the syntax for line comments and whitespace recognition.
    spaceDesc = SpaceDesc.plain.copy(
      lineCommentStart = "#",
      space = predicate.Basic(_.isWhitespace),
    ),      
    // Disables octal and hexadecimal formats for integer numbers.
    numericDesc = numeric.NumericDesc.plain.copy(
      integerNumbersCanBeOctal = false,
      integerNumbersCanBeHexadecimal = false
    ), 
    // Configures escape sequences and character/string literal rules.
    textDesc = text.TextDesc.plain.copy(
      escapeSequences = text.EscapeDesc.plain.copy(
        escBegin = '\\',
        literals = Set('\'', '\"', '\\'),
        mapping = Map (
          "0" -> 0x00,
          "b" -> 0x08,
          "t" -> 0x09,
          "n" -> 0x0a,
          "f" -> 0x0c,
          "r" -> 0x0d                
        )
      ),
      characterLiteralEnd = '\'',
      graphicCharacter = predicate.Basic(c => c >= ' ' && !Set('\\','\'', '\"').contains(c))
    )
  )
  // Creates a lexer instance with the specified description.
  private val lexer = new Lexer(desc)

  /* 
    Lexeme parsers for different constructs in the language, 
    including identifiers, numbers, negation, literals, and boolean values.
   */
  val ident: Parsley[String] = lexer.lexeme.names.identifier
  val integer = lexer.lexeme.integer.decimal32
  val NEGATE = lexer.lexeme(atomic(char('-') ~> notFollowedBy(digit)))
  val characterLiteral = lexer.lexeme.character.ascii
  val stringLiteral = lexer.lexeme.string.ascii
  val boolLiteral = ((lexer.lexeme.symbol.apply("true", "true") #> true)
                      <|> (lexer.lexeme.symbol.apply("false", "false") #> false))
  val implicits = lexer.lexeme.symbol.implicits
  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
