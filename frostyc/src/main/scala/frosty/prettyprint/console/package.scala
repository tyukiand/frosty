package frosty.prettyprint

import cats.Monad
import frosty.{TracingInterpreter, InputTracer, PhaseName}
import frosty.PhaseName.{Type => PhsType, _}
import frosty.ast._
import frosty.namespace._
import frosty.types._
import frosty.ir._
import frosty.bytecode._
import frosty.prettyprint._
import frosty.prettyprint.PrettyPrintable._
import scala.language.higherKinds

/** Pretty-printers for the console. */
package object console {
  // The package object itself contains some helper methods for a common
  // "look-and-feel" across multiple tracers.
  private[console]
  def announcePhase[M[_]: Monad](name: PhaseName, interp: TracingInterpreter[M])
  : M[Unit] = interp.println("=" * 30 + "[" + name.shortcut + "]" + "=" * 30)

  case class Noise(unwrap: Set[Class[_ <: Product]])
  case class CustomHandlers(unwrap: Map[Class[_], Any => PrettyPrintable])

  implicit val noise: Noise = Noise(Set(
    classOf[frosty.algebra.Cons[H, _, _] forSome { type H[X, Y] }],
    classOf[scala.util.Left[_, _]],
    classOf[scala.util.Right[_, _]]
  ))

  implicit val customHandlers: CustomHandlers = CustomHandlers(Map(
    classOf[frosty.Position] -> {
      case frosty.Position(f, l, c) => {
        val shortenedPath =
          f
          .path
          .split("/")
          .lastOption
          .getOrElse("<noFile>.end")
          .split("\\.")
          .headOption
          .getOrElse("<noFile>")
        Atom(s"<${shortenedPath}:${l}:${c}>")
      }
    }
  ))

  val ConsoleTracerPipeline =
    InputTracer(InputFilesTracer) >>
    DefaultListTracer[NamespaceAst[Ast, Type]](Parse) >>
    DefaultSingleEntityTracer[(List[SymbolDeclaration], List[Ast])](Resolve) >>
    DefaultListTracer[TypedAst](PhsType) >>
    DefaultListTracer[Ir](Translate) >>
    DefaultListTracer[Rich](Desugar) >>
    DefaultListTracer[Core](EliminateExpressions) >>
    DefaultSingleEntityTracer[Bytecode](EmitCode)

}