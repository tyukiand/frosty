package frosty.ir

import java.util.concurrent.atomic.AtomicInteger
import cats.Bifunctor
import frosty.namespace.AbsolutePath
import frosty.bytecode.builtin._
import scala.{Nothing => *}

sealed trait Value[+P, +N] {
  /** Promises to never actually use the second type component */
  def changeSecondType[X]: Value[P, X] = this.asInstanceOf[Value[P, X]]
}

case object UnitValue                                        extends Value[*, *]
case class B(b: Boolean)                                     extends Value[*, *]
case class I(i: Int)                                         extends Value[*, *]
case class S(value: String)                                  extends Value[*, *]
case class Freeze[+P](proc: P)                               extends Value[P, *]

sealed trait PossibleChannelName                             extends Value[*, *]

// Local variables and absolute addresses
case class LocalVar(name: String)                    extends PossibleChannelName
case class AbsolutePathName(path: AbsolutePath)      extends PossibleChannelName
// Values of special built-in channels
case class BuiltInChannelName(name: BuiltInChannel)  extends PossibleChannelName


object Value {
  implicit val bifunctor: Bifunctor[Value] = new Bifunctor[Value] {
    def bimap
      [A, B, X, Y]
      (n: Value[A, B])
      (f: A => X, _ignored: B => Y)
    : Value[X, Y] = n match {
      case x @ LocalVar(_) => x
      case g @ AbsolutePathName(_) => g
      case x @ UnitValue => x
      case x @ B(_) => x
      case x @ I(_) => x
      case x @ S(_) => x
      case Freeze(p) => Freeze(f(p))
      case b @ BuiltInChannelName(_) => b
    }
  }
}

object LocalVar {
  private val syntheticValueCounter = new AtomicInteger()
  def synthetic(hint: String): LocalVar = {
    LocalVar(s"%${hint}_${syntheticValueCounter.incrementAndGet}")
  }
}
