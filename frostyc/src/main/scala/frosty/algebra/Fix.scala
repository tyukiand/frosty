package frosty.algebra

import cats.Functor
import cats.syntax.functor._
import scala.language.higherKinds

/** Initial `F`-algebra. */
case class Fix[F[_]](unwrap: F[Fix[F]])
object Fix {

  def map[F[_]: Functor, G[_]](nat: F ~> G): Fix[F] => Fix[G] = {
    x => Fix(nat(x.unwrap.map(Fix.map(nat))))
  }
  def cata[F[_]: Functor, B](alg: F[B] => B): Fix[F] => B = {
    fixf => alg(fixf.unwrap.map(Fix.cata(alg)))
  }
  def ana[F[_]: Functor, B](coalg: B => F[B]): B => Fix[F] = {
    b => Fix(coalg(b).map(Fix.ana(coalg)))
  }

  /** Same methods, but assume `Functor` instances for `G`, not for `F`.
    * If both `F` and `G` were functors, this wouldn't matter.
    */
  object codBiased {
    def map[F[_], G[_]: Functor](nat: F ~> G): Fix[F] => Fix[G] = {
      (x: Fix[F]) => Fix(nat(x.unwrap).map(map(nat)))
    }
  }
}
