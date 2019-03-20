package frosty

import cats.Bifunctor
import cats.Bitraverse
import cats.Monad
import cats.Traverse
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.list._
import frosty.algebra.{Var, Cons, Fix, Fix2}
import frosty.ir._
import scala.util.Either
import frosty.bytecode.builtin


private[frosty]
object ExpressionEliminator extends BackendPhase[List[Rich], List[Core]] (
  PhaseName.EliminateExpressions,
  """|Flattens out all nested expressions. All non-atomic
     |expressions are replaced by variables, which eventually
     |receive their value from a parallel process.""".stripMargin
) with Closures {

  def safeCompile(input: List[Rich]): List[Core] = {
    input.par.map(safeCompileOne).toList
  }

  private def safeCompileOne(input: Rich): Core = (
    Fix2.cata
      [Proc, GenValueLike, Core, ExprContinuation]
      (input)
      (transformProc, transformGenValueLike)
  )


  /** The first half of the catamorphism; deals with the processes.
    */
  private def transformProc(p: Proc[Core, ExprContinuation]): Core = {
    // CS-XVIp109
    p match {
      case p: Parallel[Core] => mkCore(p) // contains no expressions

      case Tell(chan, msgs) => {
        materializeClosures(for {
          c <- returning("chan")(chan)
          ms <- msgs.map(returning("msg") _).sequence
        } yield mkCore(Tell(c, ms)))
      }

      case Receive(chan, varNames, body) => {
        materializeClosures(for {
          c <- returning("rcv_chan")(chan)
        } yield mkCore(Receive(c, varNames, body)))
      }

      case n: New[Core] => mkCore(n) // contains no expressions

      case Unfreeze(frozen) => {
        materializeClosures(
          for {
            f <- returning("froz")(frozen)
          } yield mkCore(Unfreeze(f))
        )
      }
    }
  }

  // Second half of the catamorphism, flattens expressions, recurses
  // into names to rewrite frozen processes.
  private def transformGenValueLike(x: GenValueLike[Core, ExprContinuation])
  : ExprContinuation = x.fold(transformExpr, transformValue)

  private def binop
    (nameHint: String, builtInChannel: builtin.BuiltInChannel)
    (a: ExprContinuation, b: ExprContinuation)
  : ExprContinuation = { resultReturnChannel => 
    materializeClosures(for {
      vA <- returning(nameHint)(a)
      vB <- returning(nameHint)(b)
    } yield {
      mkCore(Tell(
        mkCore(BuiltInChannelName(builtInChannel)),
        List(vA, vB, resultReturnChannel)
      ))
    })
  }

  /* Comment about the implementation of `binop`; Motivation for `Cl`-monad.

  (obsolete, but instructive)

    The `for-yield` code inside the `binop` method generates something 
    very similar to the following expression:

        val rA = LocalVar.synthetic(nameHint + "retChA")
        val rB = LocalVar.synthetic(nameHint + "retChB")
        val rACh = mkCore(rA)
        val rBCh = mkCore(rB)
        val eA = LocalVar.synthetic(nameHint + "exprA")
        val eB = LocalVar.synthetic(nameHint + "exprB")
        mkCore(New(
          List(rA.name, rB.name),
          mkCore(Parallel(List(
            a(rACh),
            b(rBCh),
            mkCore(Receive(rACh, List(eA.name),
              mkCore(Receive(rBCh, List(eB.name),
                  mkCore(Tell(mkCore(BuiltInChannelName(builtInChannel)), List(
                      mkCore(eA), mkCore(eB), resultReturnChannel
                  )))
              ))
            ))
          )))
        ))

    The generation of synthetic variable names is moved to the `returning`
    helper method.
    The `Cl` monad accumulates three kinds of operations:
      two receives (for rACh and rBCh),
      two parallel procs (a(rACh) and b(rBCh)),
      and it also remembers that it has to get two new channel names from `New`.
    The `materializeClosures` simply unfolds all these operations accumulated
    in the `Cl`, and wraps the `Tell` into receives, then attaches the two
    parallel fulfilling processes, and then again wraps the whole thing into a
    `New`.

  */

  private def transformExpr(e: Expr[Core, ExprContinuation])
  : ExprContinuation = {
    // CS-XVIp108
    e match {
      case Invocation(f, args) => { resultReturnChannel =>
        materializeClosures(for {
          fEvaluated <- returning("func")(f)
          argsEvaluated <- args.map(returning("arg") _).sequence
        } yield {
          mkCore(Tell(fEvaluated, argsEvaluated :+ resultReturnChannel))
        })
      }
      case AndB(a, b) => binop("and_b", builtin.AndB)(a, b)
      case AddI(a, b) => binop("add_i", builtin.AddI)(a, b)
      case SubI(a, b) => binop("sub_i", builtin.SubI)(a, b)
      case MulI(a, b) => binop("mul_i", builtin.MulI)(a, b)
      case DivI(a, b) => binop("div_i", builtin.DivI)(a, b)
      case RemI(a, b) => binop("rem_i", builtin.RemI)(a, b)
      case EqI(a, b) => binop("eq_i", builtin.EqI)(a, b)
      case ConcatS(a, b) => binop("concat_s", builtin.ConcatS)(a, b)

      case ValueBlock(procs, value) => { resultReturnChannel =>
        mkCore(Parallel(value(resultReturnChannel) :: procs))
      }

      case Await(channel) => { resultReturnChannel => 
        materializeClosures(for {
          c <- returning("awaited")(channel)
        } yield {
          val x = LocalVar.synthetic("awtRes")
          mkCore(Receive(c, List(x.name), 
            mkCore(Tell(resultReturnChannel, List(mkCore(x))))
          ))
        })
      }

      case NewExpr(names, body) => { resultReturnChannel =>
        mkCore(New(names, body(resultReturnChannel)))
      }

      case ReceiveExpr(ch, names, body) => { resultReturnChannel => 
        materializeClosures(
          for {
            c <- returning("rcv_ch")(ch)
          } yield {
            mkCore(Receive(c, names, body(resultReturnChannel)))
          }
        )
      }
      
      case sthElse => {
        System.err.println(
          "ExprElim: No implementation available yet for expression " + e
        )
        ???
      }
    }
  }

  /** Simply returns the given value to the return channel as-is.
    */
  private def transformValue(v: Value[Core, ExprContinuation])
  : ExprContinuation = {
    // TODO: how about a more sophisticated ExprContinuation that avoid 
    // generation of certain unnecessary temporary variables and channels?
    val returned = mkCore(v.changeSecondType[CoreValue])
    retCh => mkCore(Tell(retCh, List(returned)))
  }

}
