package frosty

import cats.arrow.FunctionK
import scala.language.higherKinds

/** Contains type constructors for initial F-Algebras and term algebras. */
package object algebra {
  type ~>[F[_], G[_]] = FunctionK[F, G]
  
  trait ~~>[F[_, _], G[_, _]] {
    def apply[X, Y](fxy: F[X, Y]): G[X, Y]
  }

  trait BifunctorCoCone[F[_, _], Z] 
  extends (F ~~> ({ type C[X, Y] = Z})#C) {
    def apply[X, Y](fxy: F[X, Y]): Z
  }
}
