package integrationTest

import java.io.{PrintWriter, File, FileWriter}
import java.nio.file.{StandardCopyOption, Files, Paths}
import scala.jdk.CollectionConverters._

class FilePaths(val srcFile: String, destFile: String) {
  val executableFile: String = destFile.takeWhile(_ != '.') // strip file extension
  val assemblyFile: String = executableFile + ".s"
  val resultFile: String = executableFile + ".result"
}

object FileHandler {
  def initDirectory(dirPathString: String): Unit = {
    val dirPath = Paths.get(dirPathString)
    if (Files.exists(dirPath)) {
      Files.walk(dirPath)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
    }
    Files.createDirectories(dirPath)
  }

  def getWaccFiles(folderName: String): List[String] = {
    val folder = {
      val f = new File(folderName)
      if (f.exists() && f.isDirectory()) {
        f
      } else {
        val parentFolder = new File("../" + folderName)
        if (parentFolder.exists() && parentFolder.isDirectory()) {
          parentFolder
        } else {
          throw new IllegalArgumentException("The provided path is not a valid directory.")
        }
      }
    }

    val folderPath = folder.toPath
    val stream = Files.walk(folderPath)
    try {
      stream.iterator().asScala
        .filter(file => Files.isRegularFile(file) && file.toString.endsWith(".wacc"))
        .map(path => folderPath.relativize(path).toString)
        .toList
    } finally {
      stream.close()
    }
  }

  def writeStringToFile(content: String, filePath: String): Unit = {
    val writer = new PrintWriter(new FileWriter(filePath, false))
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
  }
}