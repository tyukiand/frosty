package frosty

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.data.EitherT
import cats.instances.either._
import cats.syntax.either._
import scala.language.higherKinds

/** Description of a compiler phase.
  *
  * This will be displayed in `help`, it should enable the user to 
  * pass the right arguments to the print-option to get the relevant 
  * information.
  */
case class PhaseDescription(name: PhaseName, descr: String)

/** A compilation phase that takes `X` as input, and attempts to produce
  * an output of type `Y`. Can fail and produce list of errors instead.
  */
abstract class Phase[X, Y](val name: PhaseName, descr: String)
extends (X => CompilationErrorsOr[Y]) {
  lazy val description = PhaseDescription(name, descr)
}

/** A compiler phase that does not have to deal with any user errors.
  *
  * It assumes that all user errors have already been filtered in the
  * levels above.
  */
abstract class BackendPhase[X, Y](name: PhaseName, descr: String)
extends Phase[X, Y](name, descr) {
  def safeCompile(input: X): Y
  def apply(input: X): CompilationErrorsOr[Y] = Right(safeCompile(input))
}

trait CompilerPipeline[Src, Trg] {

  type This >: this.type <: CompilerPipeline[Src, Trg]
  type Tr <: TracerPipeline[Src, Trg, Tr]

  def run[M[_]: Monad](src: Src, tracer: Tr, intrp: TracingInterpreter[M])
  : M[CompilationErrorsOr[Trg]]

  def description: List[PhaseDescription]
  def allPhaseNames: List[PhaseName] = description.map(_.name)

  def >>[A](nextPhase: Phase[Trg, A]) = 
    CompilerPipelineCons[Trg, A, Src, This](this, nextPhase)
}

case class CompilerPipelineCons
  [A, B, Z, Comp <: CompilerPipeline[Z, A]]
  (previous: Comp, phase: Phase[A, B])
extends CompilerPipeline[Z, B] {
  type This = CompilerPipelineCons[A, B, Z, Comp]
  type Tr = TracerPipelineCons[A, B, Z, previous.Tr]
  def run[M[_]: Monad](input: Z, tracers: Tr, intrp: TracingInterpreter[M])
  : M[CompilationErrorsOr[B]] = {
    (for {
      a <- EitherT(previous.run(input, tracers.heads, intrp))
      b <- EitherT(phase(a).pure[M])
      _ <- EitherT(
             tracers
             .last
             .trace(b, intrp)
             .map(_.asRight[CompilationErrors])
           )
    } yield b).value
  }

  def description: List[PhaseDescription] =
    previous.description :+ phase.description
}

case class InputPhase[A]() extends CompilerPipeline[A, A] {
  type This = InputPhase[A]
  type Tr  = InputTracer[A]
  def run[M[_]: Monad](input: A, tracer: Tr, intrp: TracingInterpreter[M])
  : M[CompilationErrorsOr[A]] = {
    tracer.last.trace(input, intrp).map(_ => Right(input))
  }
  // TODO: this should go through the `InputTracer`, the
  // `PhaseName.Input` looks like magic constant?
  def description = List(PhaseDescription(PhaseName.Input, "reads input"))
}
