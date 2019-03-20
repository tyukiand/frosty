package frosty

import cats.data.Chain
import cats.data.Writer
import cats.Monad
import cats.syntax.writer._
import frosty.algebra.Fix2
import frosty.ir._

trait Closures {

  // An output `Chan[T]` is roughly the same as `T => Proc`
  // Therefore, `Chan[T] => Proc` is `(T => Proc) => Proc`, which is essentially
  // a continuation eventually yielding a value of type `T`.
  type ExprContinuation = CoreValue => Core

  /** A value of type `Cl[X]` is essentially something that behaves as if it
    * were an `X` as soon as `X` is enclosed in a `Core`-process, and all
    * operations stashed in the writer monad are executed on this process in
    * a particular order.
    */
  type Cl[X] = Writer[Chain[ProcTransform], X]

  /** Operation that takes a process and builds another process from it.
    * 
    * There are only three ways to do this: build an additional value receiver
    * around the process, run an additional process in parallel, or put the
    * process under a `new`-binder.
    *
    * These are the basic operations accumulated by the `Cl`-monad.
    */
  sealed trait ProcTransform

  case class AddParallelProcesses(ps: List[Core]) extends ProcTransform
  case class ListenOnChannel(c: PossibleChannelName, boundVars: List[String])
    extends ProcTransform
  case class GetNewNames(boundVars: List[String]) extends ProcTransform

  /** Given a way to construct a promise-fulfilling process that returns a value
    * to the specified return channel, creates a promise that looks like a 
    * simple local variable inside of a `Cl` context.
    *
    * It's a particularly common way to create new promises. Once we have 
    * local variables, everything else can be built on top of them.
    */
  def returning(hint: String)(cont: ExprContinuation): Cl[CoreValue] = {
    // TODO: If we had a more refined notion of `ExprContinuation`, we could 
    // avoid allocation of new channels for constants. Currently, it's somewhat
    // wasteful.
    val returnChannel = LocalVar.synthetic(s"retCh_${hint}")
    val exprReplacement = LocalVar.synthetic(s"yExpr_${hint}")
    val fulfillingProc = cont(mkCore(returnChannel))
    mkCore(exprReplacement).writer(Chain(
      ListenOnChannel(returnChannel, List(exprReplacement.name)),
      AddParallelProcesses(List(fulfillingProc)),
      GetNewNames(List(returnChannel.name))
    ))
  }

  /** Given a `CoreValue` channel (either a path or a local variable of channel
    * type) produces an expression that will eventually evaluate to the first
    * value received on this channel.
    */
  def await(channel: Cl[CoreValue]): Cl[CoreValue] = {
    channel.flatMap { coreValue =>
      val ch: PossibleChannelName = coreValue.unwrap match {
        case pcv: PossibleChannelName => pcv
        case sthElse => {
          throw new Error(
            "Fatal error. Got a value that cannot possibly be a channel " + 
            "during `await`-expression elimination: " + sthElse
          )
        }
      }

      val receivedValue = LocalVar.synthetic("await")
      val awaiting = ListenOnChannel(ch, List(receivedValue.name))
      mkCore(receivedValue).writer(Chain(awaiting))
    }
  }

  /** Appends bunch of additional processes that run in parallel without the
    * goal of fulfilling any promises.
    */
  def addParallelProcesses[X](cl: Cl[X], procs: List[Core]): Cl[X] = {
    cl.flatMap(x => x.writer(Chain(AddParallelProcesses(procs))))
  }

  /** Asks to get some `new` names as soon as possible */
  def addNewNames[X](cl: Cl[X], names: List[String]): Cl[X] = {
    cl.flatMap(x => x.writer(Chain(GetNewNames(names))))
  }

  /** Attaches `Receive`s, adds parallel processes, requests new names.
    */
  def materializeClosures(x: Cl[Core]): Core = {

    val trafos: List[ProcTransform] = x.written.toList

    val listeners: List[ListenOnChannel] =
      trafos.collect { case l: ListenOnChannel => l }

    val parallels: List[Core] = trafos
      .collect { case p: AddParallelProcesses => p.ps }
      .flatten

    val news: List[String] = trafos
      .collect { case GetNewNames(names) => names }
      .flatten

    val withReceives: Core = listeners.foldLeft(x.value) {
      case (p, ListenOnChannel(c, names)) =>
        mkCore(Receive(mkCore(c), names, p))
    }

    val withParallels = if (parallels.isEmpty) {
      withReceives
    } else {
      mkCore(Parallel(withReceives :: parallels))
    }

    val withNews = if (news.isEmpty) {
      withParallels
    } else {
      mkCore(New(news, withParallels))
    }

    withNews
  }
}