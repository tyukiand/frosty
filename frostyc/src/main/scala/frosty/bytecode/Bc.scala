package frosty.bytecode

import cats.Bifunctor
import scala.{Nothing => *}
import frosty.bytecode.builtin._

sealed trait Bc[+T, +B]
case class Parallel[T](procs: List[T])                          extends Bc[T, *]
case class Receive[T, B](channel: T, binder: B)                 extends Bc[T, B]
case class Tell[T](channel: T, messages: List[T])               extends Bc[T, *]
case class New[T, B](binder: B)                                 extends Bc[T, B]
case class Unfreeze[T](value: T)                                extends Bc[T, *]
case class Freeze[T](proc: T)                                   extends Bc[T, *]
case class PathName(path: List[String])                         extends Bc[*, *]
case object U                                                   extends Bc[*, *]
case class B(b: Boolean)                                        extends Bc[*, *]
case class I(n: Int)                                            extends Bc[*, *]
case class S(s: String)                                         extends Bc[*, *]
case class BuiltInChannelName(name: BuiltInChannel)             extends Bc[*, *]

object Bc {
  implicit object bifunctor extends Bifunctor[Bc] {
    def bimap[A, X, B, Y](a: Bc[A, X])(f: A => B, g: X => Y)
    : Bc[B, Y] = a match {
      case Parallel(procs) => Parallel(procs map f)
      case Unfreeze(v) => Unfreeze(f(v))
      case Receive(c, b) => Receive(f(c), g(b))
      case New(b) => New(g(b))
      case Tell(c, m) => Tell(f(c), m map f)
      case Freeze(p) => Freeze(f(p))
      case p @ PathName(_) => p
      case u @ U => u
      case b @ B(_) => b
      case i @ I(_) => i
      case s @ S(_) => s
      case n @ BuiltInChannelName(_) => n
    }
  }
}
