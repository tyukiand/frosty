package frosty.ir

import cats.Bifunctor
import scala.{Nothing => *}

/** Non-atomic expressions that aren't a single constant or a single name.
  */
sealed trait Expr[+P, +N] 
case class Invocation[N](f: N, args: List[N])                 extends Expr[*, N]
case class AndB[N](a: N, b: N)                                extends Expr[*, N]
case class OrB[N](a: N, b: N)                                 extends Expr[*, N]
case class NotB[N](a: N)                                      extends Expr[*, N]
case class EqU[N](a: N, b: N)                                 extends Expr[*, N]
case class EqB[N](a: N, b: N)                                 extends Expr[*, N]
case class EqI[N](a: N, b: N)                                 extends Expr[*, N]
case class EqS[N](a: N, b: N)                                 extends Expr[*, N]
case class AddI[N](a: N, b: N)                                extends Expr[*, N]
case class SubI[N](a: N, b: N)                                extends Expr[*, N]
case class MulI[N](a: N, b: N)                                extends Expr[*, N]
case class DivI[N](a: N, b: N)                                extends Expr[*, N]
case class RemI[N](a: N, b: N)                                extends Expr[*, N]
case class NegI[N](a: N)                                      extends Expr[*, N]
case class GrI[N](a: N, b: N)                                 extends Expr[*, N]
case class GeqI[N](a: N, b: N)                                extends Expr[*, N]
case class LeI[N](a: N, b: N)                                 extends Expr[*, N]
case class LeqI[N](a: N, b: N)                                extends Expr[*, N]
case class ConcatS[N](a: N, b: N)                             extends Expr[*, N]
case class IfElseE[N](c: N, t: N, e: N)                       extends Expr[*, N]
case class ValueBlock[P, N](proc: List[P], value: N)          extends Expr[P, N]
case class Await[N](channel: N)                               extends Expr[*, N]
case class NewExpr[N](names: List[String], a: N)              extends Expr[*, N]
case class ReceiveExpr[N](channel: N, names: List[String], body: N)
                                                              extends Expr[*, N]


object Expr {
  implicit val bifunctor: Bifunctor[Expr] = new Bifunctor[Expr] {
    def bimap[A, B, X, Y](e: Expr[A, B])(f: A => X, g: B => Y): Expr[X, Y] = {
      e match {
        case Invocation(f, xs) => Invocation(g(f), xs map g)
        case AndB(a, b) => AndB(g(a), g(b))
        case OrB(a, b) => OrB(g(a), g(b))
        case NotB(a) => NotB(g(a))
        case EqU(a, b) => EqU(g(a), g(b))
        case EqB(a, b) => EqB(g(a), g(b))
        case EqI(a, b) => EqI(g(a), g(b))
        case EqS(a, b) => EqS(g(a), g(b))
        case AddI(a, b) => AddI(g(a), g(b))
        case SubI(a, b) => SubI(g(a), g(b))
        case MulI(a, b) => MulI(g(a), g(b))
        case DivI(a, b) => DivI(g(a), g(b))
        case RemI(a, b) => RemI(g(a), g(b))
        case NegI(a) => NegI(g(a))
        case GeqI(a, b) => GeqI(g(a), g(b))
        case GrI (a, b) => GrI (g(a), g(b))
        case LeqI(a, b) => LeqI(g(a), g(b))
        case LeI (a, b) => LeI (g(a), g(b))
        case ConcatS(a, b) => ConcatS(g(a), g(b))
        case IfElseE(c, t, e) => IfElseE(g(c), g(t), g(e))
        case ValueBlock(ps, v) => ValueBlock(ps map f, g(v))
        case Await(c) => Await(g(c))
        case NewExpr(names, e) => NewExpr(names, g(e))
        case ReceiveExpr(c, ns, b) => ReceiveExpr(g(c), ns, g(b))
      }
    }
  }
}