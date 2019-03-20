package frosty.prettyprint.console

import cats.Monad
import cats.Foldable
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.list._
import frosty.{Tracer, TracingInterpreter, PhaseName}
import frosty.prettyprint.PrettyPrintable
import scala.language.higherKinds

case class DefaultSingleEntityTracer[E](phase: PhaseName)
  (implicit 
    noise: Noise,
    customHandlers: CustomHandlers
  )
extends Tracer[E](phase) {

  def trace[M[_]: Monad](entity: E, intrp: TracingInterpreter[M]): M[Unit] = {
    val str = 
      PrettyPrintable
      .fromAny(entity, noise.unwrap, customHandlers.unwrap)
      .prettyPrint(160)
    for {
      _ <- announcePhase(phase, intrp)
      _ <- intrp.println(str)
    } yield ()
  }
}
