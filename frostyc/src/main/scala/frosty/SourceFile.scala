package frosty

import java.io.{InputStream, FileInputStream}

sealed trait SourceCode {
  def path: String
  def loadContent: String
}

case class InMemorySnippet(pseudoPath: String, content: String)
extends SourceCode {
  def path = pseudoPath
  def loadContent = content
}

case class SourceFile(path: String) extends SourceCode {
  def inputStream: InputStream = new FileInputStream(path)
  def loadContent: String = {
    val s = io.Source.fromFile(path)
    try {
      s.getLines.mkString("\n")
    } finally {
      s.close()
    }
  }
}
