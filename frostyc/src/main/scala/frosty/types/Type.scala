package frosty.types

/** Constructor for type expressions. */
sealed trait Type
case class ChannelType(messageTypes: List[Type]) extends Type

/** Name-like entity, corresponding to a quoted process in rho-calculus. */
case object FreezeProcessType extends Type {
  override def toString = "Proc"
}
/** An entity that accepts arguments and starts a process.
  * 
  * Almost the same as a channel to which one can send messages,
  * except that it is allowed to listen on a channel, but it's not
  * allowed to listen on a `Runnable`.
  */
case class RunnableType(argTypes: List[Type]) extends Type
case class FunctionType(dom: List[Type], cod: Type) extends Type
case object UnitType extends Type
case object BooleanType extends Type
case object IntType extends Type
case object StringType extends Type

/** Active unquoted process.
  *
  * Behaves more like a statement in imperative languages, when contrasted
  * with expressions.
  */
case object ProcessStatement extends Type {
  override def toString = "Proc"
}

object Type {
  def isPrimitive(t: Type) = t match {
    case UnitType => true
    case BooleanType => true
    case IntType => true
    case _ => false
  }
}
