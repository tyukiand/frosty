package frosty

import cats.{Applicative, Bifunctor, Bitraverse, Eval}
import algebra.{Term, Binder, Cons, Var}
import scala.language.higherKinds
import cats.syntax.functor._
import frosty.types._

package object ast extends astLowPriorityImplicits01 {

  type PositionedAstC[P, B] = (Position, AstC[P, B])
  type Ast = Term[PositionedAstC, String, Position]

  def positionedAst(p: Position, a: AstC[Ast, (List[String], Ast)]): Ast = 
    Cons[PositionedAstC, String, Position](
      (p, Bifunctor[AstC].rightFunctor.map(a){ 
        case (names, body) => Binder(names, body)
      })
    )

  def positionedVariable(p: Position, name: String): Ast = 
    Var[PositionedAstC, String, Position](name, p)

  def position(ast: Ast): Position = ast match {
    case Var(_, p) => p
    case Cons(u) => u._1
  }

  type TypedAstC[P, B] = (Type, AstC[P, B])
  type TypedAst = Term[TypedAstC, String, Type]

  def typedAst(a: AstC[TypedAst, (List[String], TypedAst)], typ: Type)
  : TypedAst = Cons[TypedAstC, String, Type](
    (typ, Bifunctor[AstC].rightFunctor.map(a) {
      case (names, body) => Binder(names, body)
    })
  )

  def typedAst(name: String, typ: Type): TypedAst =
    Var[TypedAstC, String, Type](name, typ)

  def typeOf(tAst: TypedAst): Type = tAst match {
    case Var(_, t) => t
    case Cons(u) => u._1
  }


  implicit object TypedAstCBifunctor extends Bifunctor[TypedAstC] {
    def bimap[X, Y, U, V](x: TypedAstC[X, Y])(f: X => U, g: Y => V)
    : TypedAstC[U, V] = {
      val (t, ast) = x
      (t, Bifunctor[AstC].bimap(ast)(f, g))
    }
  }
}

class astLowPriorityImplicits01 {

  import ast._

  implicit object PositionedAstCBitraverse extends Bitraverse[PositionedAstC] {
    def bitraverse
      [G[_], A, B, C, D]
      (fab: PositionedAstC[A, B])
      (f: A => G[C], g: B => G[D])
      (implicit gApp: Applicative[G])
    : G[PositionedAstC[C, D]] = {
      val (p, a) = fab
      Bitraverse[AstC].bitraverse(a)(f, g).map((p, _))
    }

    def bifoldLeft[A, B, C]
      (fab: PositionedAstC[A, B], c: C)
      (f: (C, A) => C, g: (C, B) => C)
    : C = throw new NotImplementedError("Bitraverse[PositionedAstC].bifoldLeft assumed not needed")

    def bifoldRight[A, B, C]
      (fab: PositionedAstC[A, B], c: Eval[C])
      (f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C])
    : Eval[C] = throw new NotImplementedError("Bitraverse[PositionedAstC].bifoldRight assumed not needed")
  }
}