package frosty.algebra

import cats.Bifunctor
import scala.language.higherKinds
import frosty.prettyprint

@prettyprint.unwrap
case class Fix2A[A[_, _], B[_, _]](unwrap: A[Fix2A[A, B], Fix2B[A, B]])
@prettyprint.unwrap
case class Fix2B[A[_, _], B[_, _]](unwrap: B[Fix2A[A, B], Fix2B[A, B]])

object Fix2 {

  def mapRight
    [A[_, _]: Bifunctor, B[_, _], Z]
    (f2a: Fix2A[A, B])
    (coCone: BifunctorCoCone[B, Z])
  : Fix[A[?, Z]] = {
    val x: A[Fix2A[A, B], Fix2B[A, B]] = f2a.unwrap
    Fix[A[?, Z]](Bifunctor[A].bimap(x)(
      fa => mapRight(fa)(coCone),
      fb => mapRight(fb)(coCone)
    ))
  }

  def mapRight
    [A[_, _]: Bifunctor, B[_, _], Z]
    (f2b: Fix2B[A, B])
    (coCone: BifunctorCoCone[B, Z])
  : Z = coCone(f2b.unwrap)

  def cata
    [A[_, _]: Bifunctor, B[_, _]: Bifunctor, X, Y]
    (a: Fix2A[A, B])
    (ax: A[X, Y] => X, by: B[X, Y] => Y)
  : X = {
    ax(Bifunctor[A].bimap(a.unwrap)(cata(_)(ax, by), cata(_)(ax, by)))
  }

  def cata
    [A[_, _]: Bifunctor, B[_, _]: Bifunctor, X, Y]
    (b: Fix2B[A, B])
    (ax: A[X, Y] => X, by: B[X, Y] => Y)
  : Y = {
    by(Bifunctor[B].bimap(b.unwrap)(cata(_)(ax, by), cata(_)(ax, by)))
  }

  object codBiased {

    def mapLeft
      [A1[_, _], A2[_, _]: Bifunctor, B[_, _]: Bifunctor]
      (f2a: Fix2A[A1, B])
      (f: A1 ~~> A2)
    : Fix2A[A2, B] = {
      val x: A2[Fix2A[A1, B], Fix2B[A1, B]] = f(f2a.unwrap)
      Fix2A(Bifunctor[A2].bimap(x)(
        a => mapLeft(a)(f),
        b => mapLeft(b)(f)
      ))
    }


    def mapLeft
      [A1[_, _], A2[_, _]: Bifunctor, B[_, _]: Bifunctor]
      (f2b: Fix2B[A1, B])
      (f: A1 ~~> A2)
    : Fix2B[A2, B] = {
      val x: B[Fix2A[A1, B], Fix2B[A1, B]] = f2b.unwrap
      Fix2B(Bifunctor[B].bimap(x)(
        a => mapLeft(a)(f), 
        b => mapLeft(b)(f)
      ))
    }

    /* TODO: CRUFT, not needed
    def bimapM
      [A[_,_], B[_, _], X[_, _], Y, M[_]: Monad]
      (a: Fix2A[A, B])
      (
        ax: A[M[?], M[?]] ~~> M[X[?, ?]],
        by: BifunctorCoCone[B[M[?], M[?]], M[Y]]
      )
    : M[Fix[X[?, Y]]] = {
      ???
    }
    */
  }
}