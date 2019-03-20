package frosty

import cats.Bifunctor
import cats.instances.either._
import frosty.algebra.{Fix, Fix2, Fix2A, Fix2B, missingCat}

/** Contains intermediate representation language (`Ir`) and a converter
  * that transforms `ErasedAst`s into `Ir`.
  */
package object ir {

  type GenProclike[P, N] = Either[Sugar[P, N], Proc[P, N]]
  type GenValueLike[P, N] = Either[Expr[P, N], Value[P, N]]

  // These typedefs sketch out the plan:
  // - start with the most general processes and name-like expressions
  // - eliminate the syntactic sugar (can be done locally, easy)
  // - eliminate the expressions using CPS-like translation (not local)

  /** Terms with nested expressions and abstract syntactic sugar. */
  type Ir = Fix2A[GenProclike, GenValueLike]
  type IrValue = Fix2B[GenProclike, GenValueLike]

  /** Wrapper boilerplate for creating `Ir` from `Proc`. */
  def liftIr(p: Proc[Ir, IrValue]): Ir =
    Fix2A[GenProclike, GenValueLike](Right(p))
  def liftIr(s: Sugar[Ir, IrValue]): Ir =
    Fix2A[GenProclike, GenValueLike](Left(s))
  def liftIr(n: Value[Ir, IrValue]): IrValue =
    Fix2B[GenProclike, GenValueLike](Right(n))
  def liftIr(e: Expr[Ir, IrValue]): IrValue =
    Fix2B[GenProclike, GenValueLike](Left(e))

  /** Terms without sugar, but with nested expressions instead of simple names.
    */
  type Rich = Fix2A[Proc, GenValueLike]
  type RichValue = Fix2B[Proc, GenValueLike]

  def mkRich(p: Proc[Rich, RichValue]): Rich = Fix2A[Proc, GenValueLike](p)
  def mkRich(n: Value[Rich, RichValue]): RichValue = 
    Fix2B[Proc, GenValueLike](Right(n))
  def mkRich(e: Expr[Rich, RichValue]): RichValue =
    Fix2B[Proc, GenValueLike](Left(e))

  /** Core calculus with simple processes and non-nested atomic names. */
  type Core = Fix2A[Proc, Value]
  type CoreValue = Fix2B[Proc, Value]

  def mkCore(p: Proc[Core, CoreValue]): Core = Fix2A[Proc, Value](p)
  def mkCore(n: Value[Core, CoreValue]): CoreValue = Fix2B[Proc, Value](n)

  implicit val genProclikeBifunctor: Bifunctor[GenProclike] =
    missingCat.composeBifunctors[Either, Sugar, Proc]

  implicit val genValuelikeBifunctor: Bifunctor[GenValueLike] =
    missingCat.composeBifunctors[Either, Expr, Value]
}
