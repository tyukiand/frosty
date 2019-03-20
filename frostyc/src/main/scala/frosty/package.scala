import cats.data.{Chain, Validated}
import cats.syntax.validated._

package object frosty {
  type CompilationErrors = List[CompilationError]

  /** Monad for "vertical" error composition. 
    * 
    * In "vertical" direction, the behavior
    * is fail-fast, i.e. if one phase fails, the next phase is not started.
    */
  type CompilationErrorsOr[X] = Either[CompilationErrors, X]

  /** Applicative for "horizontal" error composition.
    * 
    * Single phase can collect multiple errors from different parts of the
    * program.
    */
  type ValidationErrorsOr[X] = Validated[Chain[CompilationError], X]

  private[frosty] 
  def err[A](pos: Position, msg: String): ValidationErrorsOr[A] = {
    Chain(CompilationError(pos, msg)).invalid
  }

  /** Re-wraps horizontally accumulatable applicative errors into 
    * fail-fast monadic errors.
    */
  implicit class ErrorDirectionConverter[A](veo: ValidationErrorsOr[A]) {
    def toPhaseErrors: CompilationErrorsOr[A] = veo match {
      case Validated.Invalid(errs) => Left(errs.toList)
      case Validated.Valid(result) => Right(result)
    }
  }

  type PhaseName = PhaseName.Value

  val Compiler =
    InputPhase[List[SourceFile]] >>
    Parser >>
    SymbolResolver >>
    Typer >>
    Translator >>
    Desugarer >>
    ExpressionEliminator >>
    CodeEmitter

}
