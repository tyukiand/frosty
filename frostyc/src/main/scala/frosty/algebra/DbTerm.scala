package frosty.algebra

import frosty.prettyprint
import cats.Bifunctor
import scala.language.higherKinds

/** De-Bruijn term that uses 1-based de-Bruijn indices to represent variables.
  *
  * All terms are assumed to be closed, there are no names.
  */
sealed trait DbTerm[S[_, _], I]
case class DbVar[S[_, _], I](idx: Int, info: I) extends DbTerm[S, I]

@prettyprint.unwrap
case class DbCons[S[_, _], I](unwrap: S[DbTerm[S, I], DbBinder[S, I]])
  extends DbTerm[S, I]

/** A de-Bruijn binder does not have to hold any names. It only stores the
  * number of bound variables and the body.
  */
case class DbBinder[S[_, _], I](numVars: Int, body: DbTerm[S, I])

object DbTerm {
  /** Converts first-order terms to de-Bruijn representation without names. */
  def fromTerm[S[_, _]: Bifunctor, N, I](t: Term[S, N, I]): DbTerm[S, I] = {
    def recHelper(
      t: Term[S, N, I],
      nameToBinderDepth: Map[N, Int],
      currDepth: Int
    ): DbTerm[S, I] = t match {
      case Var(name, info) => {
        val binderDepth = nameToBinderDepth(name)
        DbVar(currDepth - binderDepth, info)
      }
      case Cons(u) => {
        DbCons(Bifunctor[S].bimap(u)(
          term => recHelper(term, nameToBinderDepth, currDepth),
          binder => {
            val n = binder.names.size
            val newDepth = currDepth + n
            val newName2depth = nameToBinderDepth ++ (for {
              (n, i) <- binder.names.zipWithIndex
            } yield (n, i + currDepth))
            val newBody = recHelper(binder.body, newName2depth, newDepth)
            DbBinder(n, newBody)
          }
        ))
      }
    }
    recHelper(t, Map.empty, 0)
  }

  def foldWithIndices
    [S[_, _]: Bifunctor, I, V]
    (t: DbTerm[S, I])
    (s: S[V, (Int, V)] => V, v: (Int, I) => V)
  : V = t match {
    case DbVar(idx, info) => v(idx, info)
    case DbCons(u) => {
      s(Bifunctor[S].bimap(u)(
        subterm => foldWithIndices(subterm)(s, v),
        binder => {
          val DbBinder(n, body) = binder
          (n, foldWithIndices(body)(s, v))
        }
      ))
    }
  }
}
