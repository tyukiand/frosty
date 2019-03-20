package frosty.ir

import cats.Monad
import cats.syntax.functor._
import util.{Either, Left, Right}
import frosty.erasure._

/* TODO: Everything broken after redefining all types. Should be reparable.

case class Promise(onChan: Name, params: List[Name])

/** Monad that represents values parameterized by names that await their
  * values on channels, together with processes that will eventually send
  * results to those channels.
  */
case class ParCtx[X](
  dependencies: List[Ir],
  parameters: List[Promise],
  value: X
)

object ParCtx {
  implicit val monad: Monad[ParCtx] = new Monad[ParCtx] {
    def pure[A](a: A): ParCtx[A] = ParCtx(Nil, Nil, a)
    def flatMap[X, Y](fx: ParCtx[X])(f: X => ParCtx[Y]): ParCtx[Y] = {
      val ParCtx(moreDeps, moreParams, v2) = f(fx.value)
      ParCtx(moreDeps ::: fx.dependencies, moreParams ::: fx.parameters, v2)
    }
    def tailRecM[A, B](a: A)(f: A => ParCtx[Either[A, B]]): ParCtx[B] = {
      var depAcc = collection.mutable.ListBuffer.empty[Ir]
      var parAcc = collection.mutable.ListBuffer.empty[Promise]
      var curr: Either[A, B] = Left(a)
      while (curr.isLeft) {
        val Left(a) = curr
        val ParCtx(moreDeps, moreParams, nextA) = f(a)
        depAcc ++= moreDeps
        parAcc ++= moreParams
        curr = nextA
      }
      val Right(res) = curr
      ParCtx(depAcc.toList, parAcc.toList, res)
    }
  }


  private var syntheticNameCounter = 0
  private def syntheticName(typeHint: String): String = {
    syntheticNameCounter += 1
    s"<synth_promise_${typeHint}_${syntheticNameCounter}>"
  }

  /** Promises that variables inside of the `ParCtx` will
    * eventually be fulfilled with messages of types specified in `erasedTypes`,
    * using the process constructed specifically for those variables using
    * `fulfillingProcess`, which sends the results to the supplied return 
    * channel.
    *
    * The names of the variables are synthesized automatically.
    * The `fulfillingProcess` function gets the name of the return channel.
    */
  def promise
    (erasedTypes: List[ErasedType])
    (fulfillingProcess: Name => Ir)
  : ParCtx[List[Name]] = {
    val retChanName = Local(syntheticName("retChan"), ChannelType)
    val names = for (t <- erasedTypes) yield {
      Local(syntheticName(t.toString), t)
    }
    val promise = Promise(retChanName, names)
    val dependencies = List(fulfillingProcess(retChanName))
    val parameters = List(promise)
    ParCtx(dependencies, parameters, names)
  }

  def promise
    (erasedType: ErasedType)
    (fulfillingProcess: Name => Ir)
  : ParCtx[Name] = {
    promise(List(erasedType))(fulfillingProcess).map(_.head)
  }

  def extract(x: ParCtx[Ir]): Ir = {
    Parallel(x.parameters.foldLeft(x.value) { (inner, p) => 
      Receive(p.onChan, p.params, inner)
    } :: x.dependencies)
  }
}
*/