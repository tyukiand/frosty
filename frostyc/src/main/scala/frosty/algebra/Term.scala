package frosty.algebra

import scala.language.higherKinds
import cats.{Bifunctor, Bitraverse, Monad, Applicative}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import cats.syntax.applicative._

/** Terms with named variables.
  *
  * The shape of terms is determined by the bifunctor `S`, `N` is the type of
  * names (e.g. `String`), `I` is additional information that is attached to
  * variables.
  */
sealed trait Term[S[_, _], N, I]
case class Var[S[_, _], N, I](name: N, varInfo: I) extends Term[S, N, I]
case class Cons[S[_, _], N, I](unwrap: S[Term[S, N, I], Binder[S, N, I]])
  extends Term[S, N, I]

case class Binder[S[_, _], N, I](names: List[N], body: Term[S, N, I])
object Binder {
  def map
    [S[_, _]: Bifunctor, R[_, _], N, I, M, J]
    (b: Binder[S, N, I])
    (f: S ~~> R, g: N => M, h: I => J)
  : Binder[R, M, J] = {
    Binder(b.names map g, Term.map(b.body)(f, g, h))
  }
}

case class Env[N, V](bindings: Map[N, V]) {
  def updated(k: N, v: V) = Env(bindings + (k -> v) )
  def +(kv: (N, V)) = Env(bindings + kv)
  def ++(kvs: Seq[(N, V)]) = Env(bindings ++ kvs)
  def apply(k: N) = bindings(k)
  def get(k: N) = bindings.get(k)
}

object Env {
  def empty[N, V]: Env[N, V] = Env(Map.empty)
}

object Term {

  def fold
    [S[_, _]: Bifunctor, N, I, V]
    (t: Term[S, N, I])
    (foldCons: S[V, (List[N], V)] => V, foldVar: (N, I) => V)
  : V = t match {
    case Var(n, i) => foldVar(n, i)
    case Cons(u) => {
      foldCons(Bifunctor[S].bimap(u)(
        subterm => fold(subterm)(foldCons, foldVar),
        {
          case Binder(names, body) => (names, fold(body)(foldCons, foldVar))
        }
      ))
    }
  }

  def eval[S[_, _]: Bifunctor, N, I, V]
    (t: Term[S, N, I], env: Env[N, V] = Env.empty)
    (f: S[V, List[V] => V] => V)
  : V = t match {
    case Var(n, _) => env(n)
    case Cons(u) => {
      f(Bifunctor[S].bimap(u)(
        subterm => eval(subterm, env)(f),
        { case Binder(names, body) =>
          (values: List[V]) => eval(body, env ++ (names zip values))(f)
        }
      ))
    }
  }

  /** Evaluation for evaluation with monadic effects, but which are 
    * guaranteed not to fail.
    */
  def evalM[S[_, _]: Bitraverse, N, I, M[_]: Monad, V]
    (t: Term[S, N, I], env: Env[N, V] = Env.empty)
    (f: S[V, List[V] => M[V]] => M[V])
  : M[V] = t match {
    case Var(n, _) => Monad[M].pure(env(n))
    case Cons(u) => {
      val x: M[S[V, List[V] => M[V]]] =
        Bitraverse[S].bitraverse(u)(
          subterm => evalM(subterm, env)(f),
          binder => {
            val Binder(names, body) = binder
            Monad[M].pure {
              (values: List[V]) => evalM(body, env ++ (names zip values))(f)
            }
          }
        )
      x flatMap f
    }
  }

  /** Evaluates the term, recursively descending into subterms with an 
    * environment holding values. Allows the caller to make use of the 
    * variable names of the bound local variables.
    */
  def evalWithNames
    [S[_, _]: Bifunctor, N, I, V]
    (t: Term[S, N, I], env: Env[N, V] = Env.empty)
    (f: S[V, (List[N], List[V] => V)] => V)
  : V = t match {
    case Var(n, _) => env(n)
    case Cons(u) => {
      f(Bifunctor[S].bimap(u)(
        subterm => evalWithNames(subterm, env)(f),
        binder => {
          val Binder(names, body) = binder
          val bodyEvaluator = {
            (vs: List[V]) => evalWithNames(body, env ++ (names zip vs))(f)
          }
          (names, bodyEvaluator)
        }
      ))
    }
  }


  /** Values of this type are required by `evalWithNamesA`.
    *
    * These values have `F-Algebra`-esque feeling to them,
    * with three differences: first, everything is wrapped
    * in the error-accumulating applicative `A`; second, 
    * it's higher order, that is, the result type `V` appears
    * as function argument inside of the input type; and third,
    * it seems somewhat contaminated by names `N`. The names
    * seem not strictly necessary for anything, but not having the
    * names available would make later pretty-printing and debugging
    * unnecessarily hard, so the names are preserved too, despite
    * being unnatural and essentially irrelevant because of alpha-conversion.
    */
  type ApplicativeEvalAlgebra[S[_, _], N, A[_], V] = 
    S[A[V], (List[N], List[V] => A[V])] => A[V]

  /** Evaluates a term with applicative effect that models failure and
    * can accumulate multiple errors.
    *
    * When evaluating the binders, the caller of the method has access to
    * the names of the bound local variables.
    *
    * It might be helpful to take a look at the signature of [evalWithNames],
    * it's essentially the same, but without applicatives and 
    * error accumulation.
    */
  // TORESEARCH: I don't like how this signature is forcing me to give
  // the variable names to `f`. Do the variable names really belong there?
  // Or could one somehow disentangle this whole thing from the actual names?
  // -> Not relevant for the current implementation, but eventually interesting
  //    to think about it before writing the next compiler.
  def evalWithNamesA
    [S[_, _]: Bifunctor, N, I, A[_]: Applicative, V]
    (t: Term[S, N, I], env: Env[N, V] = Env.empty)
    (f: ApplicativeEvalAlgebra[S, N, A, V])
    // (f: S[A[V], (List[N], List[V] => A[V])] => A[V])
    (err: (I, String) => A[V])
  : A[V] = t match {
    case Var(n, p) => env.get(n).fold(err(p, "Name not found: " + n))(_.pure[A])
    case Cons(u) => {
      f(Bifunctor[S].bimap(u)(
        subterm => evalWithNamesA(subterm, env)(f)(err),
        binder => {
          val Binder(names, body) = binder
          val bodyEvaluator = { (values: List[V]) =>
            evalWithNamesA(body, env ++ (names zip values))(f)(err)
          }
          (names, bodyEvaluator)
        }
      ))
    }
  }

  def map[S[_, _]: Bifunctor, R[_, _], N, I, M, J]
    (t: Term[S, N, I])
    (f: S ~~> R, g: N => M, h: I => J)
  : Term[R, M, J] = {
    t match {
      case Var(n, i) => Var(g(n), h(i))
      case Cons(u) => Cons(f(Bifunctor[S].bimap(u)(
        subterm => Term.map(subterm)(f, g, h),
        binder => Binder.map(binder)(f, g, h)
      )))
    }
  }

  /** Performs minor modifications on the shape of terms, can accumulate
    * errors.
    *
    * Example: resolving relative names and replacing them by absolute paths.
    * This does not change the shape functor itself, but it replaces some
    * selected elements, and it can also fail.
    */
  def modifyShapeA
    [S[_, _]: Bifunctor, N, I, A[_]: Applicative]
    (t: Term[S, N, I])
    (f: Lambda[(X, Y) => S[A[X],A[Y]]] ~~> Lambda[(X, Y) => A[S[X, Y]]])
  : A[Term[S, N, I]] = t match {
    case v @ Var(_, _) => (v: Term[S, N, I]).pure[A]
    case Cons(u) =>
      f(Bifunctor[S].bimap(u)(
        trm => modifyShapeA(trm)(f),
        bnd => {
          val Binder(names, body) = bnd
          modifyShapeA(body)(f).map(x => Binder(names, x))
        }
      ))
      .map(modUnwrap => Cons(modUnwrap))
  }
}
