package wacc.codegen

import java.io.{FileWriter, File, PrintWriter}

// The FileHandler object provides utility functions for file operations.
object FileHandler {
  // Responsible for ensuring a clean, empty file for writing.  
  def initializeFile(filename: String): Boolean = {
    val file = new File(filename)
    try {
      if (file.exists() || file.createNewFile()) {
        val writer = new PrintWriter(file)
        writer.print("") // clear file content
        writer.close()
        true
      } else {
        false
      }
    } catch {
      case _: Throwable => false
    }
  }

  // Used to add content to the end of a file, useful when adding content 
  // to a file without overwriting existing content.
  def appendToFile(filename: String, content: String): Unit = {
    val writer = new FileWriter(new File(filename), true)
    try {
      writer.write(content)
    } catch {
      case _: Throwable => throw new Exception(s"Error writing to file $filename")
    } finally {
      writer.close()
    }
  }

  // Used to read the entire content of a file into a string.
  def loadFile(filename: String): String = {
    try {
      val source = scala.io.Source.fromFile(filename)
      val content = source.mkString
      source.close()
      content
    } catch {
      case _: Throwable => throw new Exception(s"Error loading file $filename")
    }
  }
}