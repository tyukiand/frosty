package frosty.ir

import cats.Bifunctor
import scala.{Nothing => *}
import cats.instances.either._

sealed trait Proc[+P, +V]
case class Parallel[P](procs: List[P])                        extends Proc[P, *]
case class Tell[V](chan: V, msgs: List[V])                    extends Proc[*, V]
case class Receive[P, V](chan: V, vars: List[String], body: P)extends Proc[P, V]
case class New[P](names: List[String], body: P)               extends Proc[P, *]
case class Unfreeze[V](frozenProc: V)                         extends Proc[*, V]

object Proc {
  implicit val bifunctor: Bifunctor[Proc] = new Bifunctor[Proc] {
    def bimap[P, V, B, M](a: Proc[P, V])(p: P => B, n: V => M): Proc[B, M] = {
      a match {
        case Parallel(ps) => Parallel(ps map p)
        case Tell(c, msgs) => Tell(n(c), msgs map n)
        case Receive(c, vs, b) => Receive(n(c), vs, p(b))
        case New(names, body) => New(names, p(body))
        case Unfreeze(frozen) => Unfreeze(n(frozen))
      }
    }
  }
}