package frosty.ir

import cats.Bifunctor

/** Process-like expressions that can easily be rewritten into processes
  * by local transformations (without promises and external processes etc.).
  */
sealed trait Sugar[+P, +N]

case class Contract[P, N](n: N, args: List[String], body: P) extends Sugar[P, N]
case class FuncDef[P, N](n: N, args: List[String], body: N) extends Sugar[P, N]
case class IfElseP[P, N](chan: N, thenProc: P, elseProc: P) extends Sugar[P, N]

object Sugar {
  implicit val bifunctor: Bifunctor[Sugar] = new Bifunctor[Sugar] {
    def bimap[P, N, B, M](a: Sugar[P, N])(p: P => B, n: N => M): Sugar[B, M] = {
      a match {
        case Contract(f, args, b) => Contract(n(f), args, p(b))
        case FuncDef(f, args, b) => FuncDef(n(f), args, n(b))
        case IfElseP(c, t, e) => IfElseP(n(c), p(t), p(e))
      }
    }
  }
}
