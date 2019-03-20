package frosty.antlr4

import org.antlr.v4.runtime.{Parser => _, _}
import org.antlr.v4.runtime.tree._
import frosty.ast._
import frosty.namespace._
import frosty.types._
import frosty.{SourceFile, SourceCode, Position, CompilationError, Parser}
import util.{Either, Left, Right}


private[frosty] object Antlr4FrostyParser {

  def parse(sourceFile: SourceFile)
  : Either[List[CompilationError], List[NamespaceAst[Ast, Type]]] = {
    // Parr, p28
    val input: CharStream = CharStreams.fromStream(sourceFile.inputStream)
    val lexer: FrostyLexer = new FrostyLexer(input)
    lexer.removeErrorListener(ConsoleErrorListener.INSTANCE)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: FrostyParser = new FrostyParser(tokens)
    parser.removeParseListeners()
    val errListener = new CompilationErrorListener(sourceFile)
    parser.addErrorListener(errListener)
    val tree: ParseTree = parser.entry()

    val errors = errListener.result

    if (errors.isEmpty) {
      val astConstructor = new AstConstructorVisitor(sourceFile)
      val VisitEntry(result) = astConstructor.visit(tree) // Parr, p42
      Right(result)
    } else {
      Left(errors)
    }
  }

  /** Attempts to parse snippet of code as a `type`.
    */
  def parseType(snippet: SourceCode): Either[List[CompilationError], Type] = {
    val input: CharStream = CharStreams.fromString(snippet.loadContent)
    val lexer: FrostyLexer = new FrostyLexer(input)
    lexer.removeErrorListener(ConsoleErrorListener.INSTANCE)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: FrostyParser = new FrostyParser(tokens)
    parser.removeParseListeners()
    val errListener = new CompilationErrorListener(snippet)
    parser.addErrorListener(errListener)
    val tree: ParseTree = parser.typ()
    val errors = errListener.result
    if (errors.isEmpty) {
      val astConstructor = new AstConstructorVisitor(snippet)
      astConstructor.visit(tree) match {
        case VisitType(t) => Right(t)
        case sthElse => throw new AssertionError(
          "The Antlr4FrostyParser should return a `VisitType(t)` here, but " +
          "unexpectedly returned: " + sthElse
        )
      }
    } else {
      Left(errors)
    }
  }

}