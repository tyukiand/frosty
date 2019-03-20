package frosty

import cats.effect.IO
import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import scala.language.higherKinds

/** Writes out a trace for a single phase. */
abstract class Tracer[B](val phaseName: PhaseName) {
  def trace[M[_]: Monad](b: B, intrp: TracingInterpreter[M]): M[Unit]
  def configure(phases: Set[PhaseName]): Tracer[B] = {
    if (phases contains phaseName) this else Tracer.noOp[B](phaseName)
  }
}

object Tracer {
  /** Tracer that does nothing at all. */
  def noOp[E](phaseName: PhaseName): Tracer[E] = new Tracer[E](phaseName) {
    def trace[M[_]: Monad](
      entity: E,
      intrp: TracingInterpreter[M]
    ): M[Unit] = Monad[M].unit
  }
}

/** Multiple `Tracer`s concatenated into a pipeline.
  *
  * Keeps track of all the types of all the intermediate results through
  * hlist-like typelevel representation.
  */
trait TracerPipeline[Z, B, This <: TracerPipeline[Z, B, This]] { self: This =>
  // type This >: this.type <: TracerPipeline[Z, B]
  def >>[C](next: Tracer[C]) = TracerPipelineCons[B, C, Z, This](this, next)
  def last: Tracer[B]

  /** Replaces some of the `Tracer`s by no-ops if they are not in the 
    * set of selected phases.
    */
  def configure(selectedPhases: Set[PhaseName]): This
}

/** Appends one more [Tracer] to a [TracerPipeline]. */
case class TracerPipelineCons
  [A, B, Z, Tr <: TracerPipeline[Z, A, Tr]]
  (heads: Tr, tracer: Tracer[B])
extends TracerPipeline[Z, B, TracerPipelineCons[A, B, Z, Tr]]{
  // type This = TracerPipelineCons[A, B, Z, Tr]
  def last: Tracer[B] = tracer
  def configure(phases: Set[PhaseName]): TracerPipelineCons[A, B, Z, Tr] = {
    TracerPipelineCons(heads.configure(phases), tracer.configure(phases))
  }
}

/** The `Nil` of a [TracerPipeline] that starts with the input. */
case class InputTracer[A](tracer: Tracer[A]) 
extends TracerPipeline[A, A, InputTracer[A]] {
  // type This = InputTracer[A]
  def last = tracer
  def configure(phases: Set[PhaseName]): InputTracer[A] = {
    InputTracer(tracer.configure(phases))
  }
}

/** Tagless-final-style algebra passed to `Tracer`s in each phase,
  * through which the tracers can write some output to stdout, or into a file,
  * or maybe into an in-memory list.
  */
trait TracingInterpreter[M[_]] {
  def print(s: String): M[Unit]
  def println(s: String)(implicit mon: Monad[M]): M[Unit] = for {
    _ <- print(s)
    _ <- print("\n")
  } yield ()
}

/** Implementation of a [TracingInterpreter] that simply dumps everything to
  * STDOUT.
  */
object ConsoleIoTracingInterpreter extends TracingInterpreter[IO] {
  def print(s: String): IO[Unit] = IO { System.out.print(s) }
}

