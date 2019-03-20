package frosty.algebra

import scala.language.higherKinds
import cats.Bifunctor
import cats.Monoid

object missingCat {

  type BifunctorComposition[F[_, _], G[_, _], H[_, _], X, Y] = 
    F[G[X, Y], H[X, Y]]

  def composeBifunctors
    [F[_, _], G[_, _], H[_, _]]
    (implicit fb: Bifunctor[F], gb: Bifunctor[G], hb: Bifunctor[H])
  : Bifunctor[BifunctorComposition[F, G, H, ?, ?]] = {
    new Bifunctor[BifunctorComposition[F, G, H, ?, ?]] {
      def bimap[X, Y, V, W](a: F[G[X, Y], H[X, Y]])(xv: X => V, yw: Y => W)
      : F[G[V, W], H[V, W]] = {
        fb.bimap(a)(
          g => gb.bimap(g)(xv, yw),
          h => hb.bimap(h)(xv, yw)
        )
      }
    }
  }

  implicit def productMonoid[A, B](implicit ma: Monoid[A], mb: Monoid[B])
  : Monoid[(A, B)] = new Monoid[(A, B)] {
    def empty = (ma.empty, mb.empty)
    def combine(t1: (A, B), t2: (A, B)): (A, B) = 
      (ma.combine(t1._1, t2._1), mb.combine(t1._2, t2._2))
  }
}