package wacc.errors

import java.io.{File}
import scala.io.{Source, BufferedSource}
import WaccError._

object PrettyPrint {

  private val CONTEXT_LINES = 1

  /**
   * Main function to pretty print the error report. This function takes an error report and
   * handles it based on its type, either printing a specific message for known types or
   * iterating through and printing all errors for generic reports.
   * 
   * @param errorReport The error report to be pretty-printed.
   */
  def prettyPrint(errorReport: WaccErrorReport) = println(prettyString(errorReport))

  def prettyString(errorReport: WaccErrorReport): String = errorReport match {
    case FileNotFound(filename) => s"${errorReport.filename} was not found"
    case ArgumentEmpty => "No arguments were provided"
    case UnexpectedError(details) => s"An unexpected error occurred:\n\t$details"
    case _ => {
      s"Error found in ${errorReport.filename}:\n" +
        s"${errorReport.errorInfos.map(printWaccError(_, errorReport.filename)).mkString("\n\n")}"
    }
  }

   /**
    * Formats a single WaccError into a string with detailed information about the error,
    * including its type, position, context, and specifics about what was unexpected and expected.
    * 
    * @param waccError The WaccError to format.
    * @param filename The filename where the error occurred for source code extraction.
    * @return A string representing the formatted error.
    */
  private def printWaccError(waccError: WaccError, filename: String): String = {
    var errorString = s"${waccError.errorType} error at (${waccError.pos._1}, ${waccError.pos._2})"

    errorString += formatOption(" in", waccError.context, "") + ":\n"
    errorString += formatOption("\tUnexpected:",waccError.unexpected, "\n")
    errorString += formatOption("\tExpected:", waccError.expected, "\n")
    errorString += formatOption("\t", waccError.reason, "\n")
    errorString += s"${printErrorLines(waccError.pos, filename, waccError.width)}\n"

    errorString
  }

  /**
   * Helper method to format an optional string with a prefix and postfix, if the string is present.
   * 
   * @param prefix The prefix to prepend to the string if present.
   * @param s The optional string to format.
   * @param postfix The postfix to append to the string if present.
   * @return A formatted string, or an empty string if the optional is None.
   */
  private def formatOption(prefix: String, s: Option[String], postfix: String): String = {
    s match {
      case Some(str) => s"$prefix $str$postfix"
      case None => s""
    }
  }

  /**
   * Formats an optional expected value for error messages. If the expected value matches a known pattern 
   * (e.g., "fst, identifier, snd"), it converts it into a more user-friendly form (e.g., "Pair"). 
   * Otherwise, it simply attaches a prefix and postfix to the expected value if present.
   *
   * @param prefix The text to add before the expected value.
   * @param s The optional expected value, which may be a specific pattern or any string.
   * @param postfix The text to add after the expected value.
   * @return A formatted string based on the expected value, or an empty string if none is provided.
   */
  private def formatExpected(prefix: String, s: Option[String], postfix: String): String = {
    s match {
      case Some("fst, identifier, snd") => s"$prefix Pair $postfix"
      case Some(str) => s"$prefix $str $postfix"
      case None => s""
    }
  }

  /**
   * Formats lines of the source code around the error location, including highlighting the error
   * position with carets (^). It uses CONTEXT_LINES to determine how many context lines to print.
   * 
   * @param pos The position (line and column) of the error in the source code.
   * @param filename The name of the file containing the source code.
   * @param width The width of the error indicator in characters.
   * @return A string representing the source code lines around the error, with highlighting.
   */
  private def printErrorLines(pos: (Int, Int), filename: String, width: Int): String = {
    var errorLines = ""

    for (i <- -CONTEXT_LINES to CONTEXT_LINES) {
      if (pos._1 + i >= CONTEXT_LINES) {
        errorLines += s"${pos._1 + i}\t |${getLine(filename, pos._1 + i).getOrElse("")}\n"
        if (i == 0) {
          errorLines += s"${pos._1 + i}".map(_ => ' ')
          errorLines += s"\t |${" " * (pos._2 - CONTEXT_LINES)}"
          for (j <- CONTEXT_LINES to width) {
            errorLines += s"^"
          }
          errorLines += s"\n"
        }
      }
    }

    errorLines
  }

  /**
   * Retrieves a specific line of text from a file by its line number.
   * 
   * @param filename The name of the file to read from.
   * @param lineNumber The line number of interest.
   * @return An Option containing the line of text if found, or None if not found or in case of an error.
   */
  private def getLine(filename: String, lineNumber: Int): Option[String] = {
    val source: BufferedSource = Source.fromFile(filename)
    try {
      val lines: Iterator[String] = source.getLines()

      // Convert the iterator to a sequence to get its length
      val lineSeq = lines.toSeq

      if (lineNumber > 0 && lineNumber <= lineSeq.length) {
        Some(lineSeq(lineNumber - CONTEXT_LINES))
      } else {
        None
      }
    } catch {
      case e: Exception => {
        None
      }
    } finally {
      source.close()
    }
  }
}
