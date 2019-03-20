package frosty

import frosty.algebra.{~>, Fix2, Fix2B}
import frosty.ir._
import frosty.bytecode.builtin._
import scala.util.{Either, Left, Right}


object Desugarer extends BackendPhase[List[Ir], List[Rich]](
  PhaseName.Desugar,
  "Expands process-like abstract syntactic sugar by simple local substitutions."
) {

  private def desugar(s: Sugar[Rich, RichValue]): Rich = s match {
    case Contract(name, args, body) => {
      // CS-XVIp91
      val replicatorChannel = LocalVar.synthetic("replicator")
      val replica = LocalVar.synthetic("replica")
      val replicator: Rich = 
        mkRich(Receive(
          mkRich(replicatorChannel),
          List(replica.name),
          mkRich(Parallel(List(
            mkRich(Receive(
              name,
              args,
              mkRich(Parallel(List(
                mkRich(Unfreeze(mkRich(replica))),
                body
              )))
            )),
            mkRich(Tell(
              mkRich(replicatorChannel),
              List(mkRich(replica))
            ))
          )))
        ))
      mkRich(New(
        List(replicatorChannel.name),
        mkRich(Parallel(List(
          replicator,
          mkRich(Tell(mkRich(replicatorChannel), List(
            mkRich(Freeze(replicator))
          )))
        )))
      ))
    }
    case FuncDef(name, args, body) => {
      val returnChannel = LocalVar.synthetic("retChn")
      // rewrite function as special case of a funny contract, invoke `desugar`
      // on that.
      desugar(Contract(
        name,
        args :+ returnChannel.name,
        mkRich(Tell(mkRich(returnChannel), List(body)))
      ))
    }
    case IfElseP(c, t, e) => {
      mkRich(Tell(
        mkRich(BuiltInChannelName(If)),
        List(c, mkRich(Freeze(t)), mkRich(Freeze(e)))
      ))
    }
  }
  
  private def transformGenProcLike(gpl: GenProclike[Rich, RichValue]): Rich = {
    gpl.fold(desugar, mkRich(_))
  }

  private def safeCompileOne(ir: Ir): Rich = {
    (Fix2.cata
      [GenProclike, GenValueLike, Rich, RichValue]
      (ir)
      (
        transformGenProcLike,
        (gnl: GenValueLike[Rich, RichValue]) => Fix2B(gnl)
      )
    )
  }

  def safeCompile(input: List[Ir]): List[Rich] = {
    input.par.map(safeCompileOne).toList
  }
}
