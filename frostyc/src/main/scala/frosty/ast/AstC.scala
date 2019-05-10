package frosty.ast

import scala.language.higherKinds
import cats.{Applicative, Bifunctor, Bitraverse, Eval, Monoid, Traverse}
import cats.syntax.monoid._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.instances.list._
import scala.{Nothing => *}
import frosty.types.Type
import frosty.namespace.{AbsolutePath, RelativePath}
import frosty.prettyprint

/** Abstract syntax tree constructor, to be used as shape-parameter in
  * [algebra.Type].
  */
sealed trait AstC[+P, +B]

// * - shortcut for `Nothing`
// [*, *] - constants
// [P, *] - operators
// [P, B] - stuff with binders
// [*, B] - cannot occur
case class Parallel[+P](procs: List[P])                  extends AstC[P, *]
case class Tell[+P](chan: P, messages: List[P])          extends AstC[P, *]
case class Receive[+P, +B](chan: P, body: B)             extends AstC[P, B]
case class Invocation[+P](f: P, args: List[P])           extends AstC[P, *]
case class Ascription[+P](proc: P, typ: Type)            extends AstC[P, *]
@prettyprint.unwrap
case class AbsoluteName(path: AbsolutePath)              extends AstC[*, *]
@prettyprint.unwrap
case class RelativeName(path: RelativePath)              extends AstC[*, *]
case class New[+B](types: List[Type], body: B)           extends AstC[*, B]
case class Contract[+P, +B](name: P, body: B)            extends AstC[P, B]
case class Def[+P, +B](name: P, body: B)                 extends AstC[P, B]
case class Await[+P](channel: P)                         extends AstC[P, *]
case object UnitValue                                    extends AstC[*, *]
@prettyprint.unwrap
case class B(b: Boolean)                                 extends AstC[*, *]
case class IfElse[+P](cond: P, thenProc: P, elseProc: P) extends AstC[P, *]
case class Not[+P](b: P)                                 extends AstC[P, *]
case class And[+P](a: P, b: P)                           extends AstC[P, *]
case class Or[+P] (a: P, b: P)                           extends AstC[P, *]
case class Eq[+P] (a: P, b: P)                           extends AstC[P, *]
@prettyprint.unwrap
case class I(i: String)                                  extends AstC[*, *]
case class Add[+P](a: P, b: P)                           extends AstC[P, *]
case class Sub[+P](a: P, b: P)                           extends AstC[P, *]
case class Mul[+P](a: P, b: P)                           extends AstC[P, *]
case class Div[+P](a: P, b: P)                           extends AstC[P, *]
case class Rem[+P](a: P, b: P)                           extends AstC[P, *]
case class Neg[+P](a: P)                                 extends AstC[P, *]
@prettyprint.unwrap
case class S(s: String)                                  extends AstC[*, *]
case class Freeze[+P](proc: P)                           extends AstC[P, *]
case class Unfreeze[+P](proc: P)                         extends AstC[P, *]
case class Gr[+P](a: P, b: P)                            extends AstC[P, *]
case class Geq[+P](a: P, b: P)                           extends AstC[P, *]
case class Le[+P](a: P, b: P)                            extends AstC[P, *]
case class Leq[+P](a: P, b: P)                           extends AstC[P, *]

object AstC {
  
  implicit val bitraverse: Bitraverse[AstC] = new Bitraverse[AstC]{
    override def bimap
      [P, B, Q, C]
      (a: AstC[P, B])
      (f: P => Q, g: B => C)
    : AstC[Q, C] = a match {
      case Parallel(procs) => Parallel(procs map f)
      case Tell(chan, messages) => Tell(f(chan), messages map f)
      case Receive(chan, body) => Receive(f(chan), g(body))
      case Invocation(func, args) => Invocation(f(func), args map f)
      case Ascription(proc, typ) => Ascription(f(proc), typ)
      case n @ AbsoluteName(path) => n
      case n @ RelativeName(path) => n
      case New(types, body) => New(types, g(body))
      case Contract(proc, body) => Contract(f(proc), g(body))
      case Def(name, body) => Def(f(name), g(body))
      case Await(ch) => Await(f(ch))
      case u @ UnitValue => u
      case b @ B(_) => b
      case IfElse(cond, thenProc, elseProc) => 
        IfElse(f(cond), f(thenProc), f(elseProc))
      case Not(b) => Not(f(b))
      case And(a, b) => And(f(a), f(b))
      case Or (a, b) => Or(f(a), f(b))
      case Eq (a, b) => Eq(f(a), f(b))
      case i @ I(_) => i
      case Add(a, b) => Add(f(a), f(b))
      case Sub(a, b) => Sub(f(a), f(b))
      case Mul(a, b) => Mul(f(a), f(b))
      case Div(a, b) => Div(f(a), f(b))
      case Rem(a, b) => Rem(f(a), f(b))
      case Neg(a) => Neg(f(a))
      case Gr(a, b) => Gr(f(a), f(b))
      case Geq(a, b) => Geq(f(a), f(b))
      case Le(a, b) => Le(f(a), f(b))
      case Leq(a, b) => Leq(f(a), f(b))
      case s @ S(_) => s
      case Freeze(p) => Freeze(f(p))
      case Unfreeze(p) => Unfreeze(f(p))
    }

    def bitraverse
      [G[_], A, B, C, D]
      (fab: AstC[A, B])
      (f: A => G[C], g: B => G[D])
      (implicit gApp: Applicative[G])
    : G[AstC[C, D]] = fab match {
      case Parallel(ps) => ps traverse f map Parallel.apply
      case Tell(ch, ms) => (f(ch), ms traverse f) mapN Tell.apply
      case Receive(c, b) => (f(c), g(b)) mapN Receive.apply
      case Invocation(func, args) => 
        (f(func), args traverse f) mapN Invocation.apply
      case Ascription(proc, typ) => f(proc).map(Ascription(_, typ))
      case n @ AbsoluteName(path) => (n: AstC[C, D]).pure[G]
      case n @ RelativeName(path) => (n: AstC[C, D]).pure[G]
      case New(types, body) => g(body).map(New(types, _))
      case Contract(proc, body) => (f(proc), g(body)) mapN Contract.apply
      case Def(name, body) => (f(name), g(body)) mapN Def.apply
      case Await(ch) => f(ch) map Await.apply
      case u @ UnitValue => (u: AstC[C, D]).pure[G]
      case b @ B(_) => (b: AstC[C, D]).pure[G]
      case IfElse(c, t, e) => (f(c), f(t), f(e)) mapN IfElse.apply
      case Not(b) => f(b) map Not.apply
      case And(a, b) => (f(a), f(b)) mapN And.apply
      case Or (a, b) => (f(a), f(b)) mapN Or.apply
      case Eq (a, b) => (f(a), f(b)) mapN Eq.apply
      case i @ I(_) => (i: AstC[C, D]).pure[G]
      case Add(a, b) => (f(a), f(b)) mapN Add.apply
      case Sub(a, b) => (f(a), f(b)) mapN Sub.apply
      case Mul(a, b) => (f(a), f(b)) mapN Mul.apply
      case Div(a, b) => (f(a), f(b)) mapN Div.apply
      case Rem(a, b) => (f(a), f(b)) mapN Rem.apply
      case Neg(a) => f(a) map Neg.apply
      case Gr(a, b) =>  (f(a), f(b)) mapN Gr.apply
      case Geq(a, b) => (f(a), f(b)) mapN Geq.apply
      case Le(a, b) =>  (f(a), f(b)) mapN Le.apply
      case Leq(a, b) => (f(a), f(b)) mapN Leq.apply
      case s @ S(_) => (s: AstC[C, D]).pure[G]
      case Freeze(p) => f(p) map Freeze.apply
      case Unfreeze(p) => f(p) map Unfreeze.apply
    }

    def bifoldLeft[A, B, C]
      (fab: AstC[A, B], c: C)
      (f: (C, A) => C, g: (C, B) => C)
    : C = ??? /* Leaky interface from scala cats? fab match {
      case Parallel(ps) => ps.foldLeft(c)(f)
      case Tell(chan, m) => m.foldLeft(f(c, chan))(f)
      case Receive(chn, bdy) => g(f(c, chn), bdy)
      case Invocation(r, args) => args.foldLeft(f(c, r))(f)
      case Ascription(p, _) => f(c, p)
      case _ => ???
      /* TODO: currently not needed; Why is it even in the interface?
      case g @ GlobalName(_) => g
      case New(args, body) => New(args, f(body))
      case Contract(n, args, b) => Contract(f(n), args, f(b))
      case Def(n, args, b) => Def(f(n), args, f(b))
      case u @ UnitValue => u
      case b @ B(_) => b
      case IfElse(c, t, e) => IfElse(f(c), f(t), f(e))
      case Not(b) => Not(f(b))
      case And(a, b) => And(f(a), f(b))
      case Or(a, b) => Or(f(a), f(b))
      case Eq(a, b) => Eq(f(a), f(b))
      case i @ I(_) => i
      case Add(a, b) => Add(f(a), f(b))
      case Sub(a, b) => Sub(f(a), f(b))
      case Mul(a, b) => Mul(f(a), f(b))
      case Div(a, b) => Div(f(a), f(b))
      case Neg(a) => Neg(f(a))
      case s @ S(_) => s
      case v @ Var(_) => v
      */
    } */

    def bifoldRight[A, B, C]
      (fab: AstC[A, B], c: Eval[C])
      (f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C])
    : Eval[C] = ??? // TODO: not needed.
  }

  /*
  def monoidFAlgebra[T, M: Monoid]: AstC[T, M, *] => M = _ match {
    case Parallel(ps) => Monoid[M].combineAll(ps)
    case Tell(c, m) => c |+| Monoid[M].combineAll(m)
    case Receive(c, _, b) => c |+| b
    case Invocation(g, args) => g |+| Monoid[M].combineAll(args)
    case Return(res) => res
    case Ascription(p, _) => p
    case g @ GlobalName(_) => Monoid[M].empty
    case New(_, body) => body
    case Contract(n, _, b) => n |+| b
    case Def(n, _, b) => n |+| b
    case u @ UnitValue => Monoid[M].empty
    case b @ B(_) => Monoid[M].empty
    case IfElse(c, t, e) => c |+| t |+| e
    case Not(b) => b
    case And(a, b) => a |+| b
    case Or(a, b) => a |+| b
    case Eq(a, b) => a |+| b
    case i @ I(_) => Monoid[M].empty
    case Add(a, b) => a |+| b
    case Sub(a, b) => a |+| b
    case Mul(a, b) => a |+| b
    case Div(a, b) => a |+| b
    case Neg(a) => a
    case s @ S(_) => Monoid[M].empty
    case v @ Var(_) => Monoid[M].empty
  }

  def monoidFAlgebra[F[_], T, M: Monoid](extract: F[M] => (M, AstC[T, M, *]))
  : F[M] => M = {
    val monFAlg = monoidFAlgebra[T, M, *]
    fm => {
      val (m, a) = extract(fm)
      m |+| monFAlg(a)
    }
  }
  */

}