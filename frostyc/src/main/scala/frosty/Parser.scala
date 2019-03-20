package frosty

import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.applicative._
import java.io.InputStream
import frosty.antlr4.Antlr4FrostyParser
import frosty.ast._
import frosty.namespace._
import frosty.types._
import scala.language.higherKinds
import scala.util.Either
import scala.collection.mutable.ListBuffer

object Parser extends Phase[
  List[SourceFile],
  List[NamespaceAst[Ast, Type]]
](
  PhaseName.Parse,
  "Reads text, builds the abstract syntax tree."
) {
  def apply(input: List[SourceFile])
  : CompilationErrorsOr[List[NamespaceAst[Ast, Type]]] = {

    // mutable collections so we don't have to traverse 
    // the list of either-or's two times.
    val allErrors = ListBuffer.empty[CompilationError]
    val allNsAsts = ListBuffer.empty[NamespaceAst[Ast, Type]]
    input
      .par
      .map(Antlr4FrostyParser.parse)
      .foreach { _ match {
        case Right(r) => allNsAsts ++= r
        case Left(cErrs) => allErrors ++= cErrs
      }}

    if (allErrors.isEmpty) {
      Right(allNsAsts.toList)
    } else {
      Left(allErrors.toList)
    }
  }
}
