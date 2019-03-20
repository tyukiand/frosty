package frosty.prettyprint.console

import cats.Monad
import cats.Foldable
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.instances.list._
import frosty.{Tracer, TracingInterpreter, PhaseName}
import frosty.SourceFile
import scala.language.higherKinds

object InputFilesTracer extends Tracer[List[SourceFile]](PhaseName.Input) {
  def trace[M[_]: Monad](
    files: List[SourceFile],
    intrp: TracingInterpreter[M]
  ): M[Unit] = {
    for {
      _ <- announcePhase(phaseName, intrp)
      _ <- Foldable[List].foldM[M, SourceFile, Unit](files, ()){
        (_, f) => intrp.println(f.path)
      }
    } yield ()
  }
}