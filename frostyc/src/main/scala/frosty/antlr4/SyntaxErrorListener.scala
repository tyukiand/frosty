package frosty.antlr4

import org.antlr.v4.runtime._
import frosty.{CompilationError, Position, SourceCode}
import collection.mutable.ListBuffer

private[antlr4] class CompilationErrorListener(sourceCode: SourceCode) 
extends BaseErrorListener {

  private val buf = ListBuffer.empty[CompilationError]

  override def syntaxError(
    recognizer: Recognizer[_, _],
    offendingSymbol: Any,
    line: Int,
    charPositionInLine: Int,
    msg: String,
    e: RecognitionException
  ): Unit = {
    val pos = Position(sourceCode, line, charPositionInLine)
    val e = CompilationError(pos, msg)
    buf += e
  }

  def result: List[CompilationError] = buf.result
}