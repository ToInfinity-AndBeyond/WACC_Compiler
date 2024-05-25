package wacc.errors

import java.io.File
import parsley.errors.ErrorBuilder

object WaccError {
  // Represents a report of errors encountered during the compilation or interpretation process.
  class WaccErrorReport(val filename: String, val errorInfos: List[WaccError])

  // Represents a case where no arguments are provided.
  object ArgumentEmpty extends WaccErrorReport("", List())
  
  // Represents a file not found error with the specified filename.
  case class FileNotFound(override val filename: String) extends WaccErrorReport(filename, List())

  case class UnexpectedError(details: String) extends WaccErrorReport("", List())
  // The default width used for displaying errors, typically in characters.
  val DEFAULT_WIDTH = 1

  // An abstract class representing a generic WACC error with common attributes.
  sealed abstract class WaccError(
    val errorType: String,
    val context: Option[String],
    val unexpected: Option[String],
    val expected: Option[String],
    val reason: Option[String],
    val pos: (Int, Int),
    val width: Int
  ) {
    def addContext(string: String): WaccError = {
      val newContext = this.context match {
        case None => Some(string)
        case Some(value) => Some(value + "\n" + string)
      }
      this match {
        case TypeError(_, u, e, r, p) => new TypeError(newContext, u, e, r, p)
        case ScopeError(_, u, e, r, p) => new ScopeError(newContext, u, e, r, p)
        case OtherError(et, _, u, e, r, p) => new OtherError(et, newContext, u, e, r, p)
        case SyntaxError(_, u, e, r, p, w) => new SyntaxError(newContext, u, e, r, p, w)
      }
    }
  }

  // A specific type of error related to type mismatches.
  case class TypeError(
    override val context: Option[String],
    override val unexpected: Option[String],
    override val expected: Option[String],
    override val reason: Option[String],
    override val pos: (Int, Int)
  ) extends WaccError("Type", context, unexpected, expected, reason, pos, DEFAULT_WIDTH)

  // A specific type of error related to scope issues, such as undefined variables.
  case class ScopeError(
    override val context: Option[String],
    override val unexpected: Option[String],
    override val expected: Option[String],
    override val reason: Option[String],
    override val pos: (Int, Int)
  ) extends WaccError("Scope", context, unexpected, expected, reason, pos, DEFAULT_WIDTH)

  // Represents other types of errors not covered by the more specific error classes.
  case class OtherError(
    override val errorType: String,
    override val context: Option[String],
    override val unexpected: Option[String],
    override val expected: Option[String],
    override val reason: Option[String],
    override val pos: (Int, Int)
  ) extends WaccError(errorType, context, unexpected, expected, reason, pos, DEFAULT_WIDTH)

  // Represents syntax errors, typically related to violations of the language grammar.
  case class SyntaxError(
    override val context: Option[String],
    override val unexpected: Option[String],
    override val expected: Option[String],
    override val reason: Option[String],
    override val pos: (Int, Int),
    override val width: Int
  ) extends WaccError("Syntax", context, unexpected, expected, reason, pos, width)

  // Helper case class for aggregating information about an error.
  protected case class ErrorInfo(
    unexpected: Option[String],
    expecteds: Set[String],
    reasons: Set[String],
    width: Int
  )

  // Converts a set of strings into a readable string, used for formatting expected values in error messages.
  private def setToString(ss: Set[String]): Option[String] = ss.filterNot(_.isEmpty).toList match {
    case Nil => None
    case head :: Nil => Some(head)
    case head :: next :: Nil => Some(s"$head or $next")
    case head :: next => Some(s"${next.init.mkString(", ")}, or ${next.last}")
  }

  // Error builder used by parsley to build WaccError object
  abstract class WaccErrorBuilder extends ErrorBuilder[WaccError] {
    override def format(pos: Position, source: Source, errorInfo: ErrorInfoLines): WaccError = {
      SyntaxError(
        None,
        errorInfo.unexpected,
        setToString(errorInfo.expecteds),
        setToString(errorInfo.reasons),
        pos,
        errorInfo.width
      )
    }
    type Position = (Int, Int)
    override def pos(line: Int, col: Int): Position = (line, col)

    type Source = String
    override def source(sourceName: Option[String]): Source = sourceName.getOrElse("")

    type ErrorInfoLines = ErrorInfo
    override def vanillaError(
      unexpected: UnexpectedLine,
      expected: ExpectedLine,
      reasons: Messages,
      width: LineInfo
    ): ErrorInfoLines = {
      ErrorInfo(unexpected, expected, reasons, width)
    }
    override def specializedError(msgs: Messages, width: LineInfo): ErrorInfoLines = {
      ErrorInfo(None, Set(), msgs, width)
    }

    type ExpectedItems = Set[Item]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    type Messages = Set[Message]
    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    type UnexpectedLine = Option[Item]
    override def unexpected(item: Option[Item]): UnexpectedLine = item
    type ExpectedLine = ExpectedItems
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message   = msg

    type LineInfo = Int
    override def lineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int,
      errorWidth: Int
    ): LineInfo = {
      errorWidth
    }

    override val numLinesBefore: Int = 0
    override val numLinesAfter: Int  = 0

    type Item       = String
    type Raw        = String
    type Named      = String
    type EndOfInput = String
    override def raw(item: String): Raw     = item
    override def named(item: String): Named = item
    override val endOfInput: EndOfInput     = "end of input"
  }
}
