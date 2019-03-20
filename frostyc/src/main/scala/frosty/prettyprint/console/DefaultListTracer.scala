package frosty.prettyprint.console

import cats.Monad
import cats.Foldable
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.list._
import frosty.{Tracer, TracingInterpreter, PhaseName}
import frosty.prettyprint.PrettyPrintable
import scala.language.higherKinds

case class DefaultListTracer[E]
  (phase: PhaseName)
  (implicit 
    noise: Noise,
    customHandlers: CustomHandlers
  )
extends Tracer[List[E]](phase) {

  def trace[M[_]: Monad](
    list: List[E],
    intrp: TracingInterpreter[M]
  ): M[Unit] = {
    for {
      _ <- announcePhase(phase, intrp)
      _ <- Foldable[List].foldM[M, E, Unit](list, ()){
        (_, a) => 
        val str = 
          PrettyPrintable
          .fromAny(a, noise.unwrap, customHandlers.unwrap)
          .prettyPrint(160)
        for {
          _ <- intrp.println("-" * 80)
          _ <- intrp.println(str)
        } yield ()
      }
    } yield ()
  }
}