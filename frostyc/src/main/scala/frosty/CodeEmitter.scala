package frosty

import frosty.algebra.{Term, Binder, DbTerm, Fix2}
import frosty.ir._
import frosty.{bytecode => bc}
import frosty.bytecode.{Bytecode, NamedBc, mkBc, mkNamedBc}

object CodeEmitter extends BackendPhase[List[Core], Bytecode](
  PhaseName.EmitCode,
  "Converts terms of the core calculus into bytecode in VM format."
) {
  def safeCompile(input: List[Core]): Bytecode = {
    mkBc(bc.Parallel(input.par.map(safeCompileOne).toList))
  }

  private def safeCompileOne(input: Core): Bytecode = {
    val named: NamedBc = (
      Fix2.cata
      [Proc, Value, NamedBc, NamedBc]
      (input)
      (procToTerm, valueToTerm)
    )
    DbTerm.fromTerm(named)
  }

  private def procToTerm(proc: Proc[NamedBc, NamedBc]): NamedBc = proc match {
    case Parallel(procs) => mkNamedBc(bc.Parallel(procs))
    case Tell(c, msgs) => mkNamedBc(bc.Tell(c, msgs))
    case Receive(c, names, bod) =>
      mkNamedBc(bc.Receive(c, Binder(names, bod)))

    case New(names, body) => mkNamedBc(bc.New(Binder(names, body)))
    case Unfreeze(p) => mkNamedBc(bc.Unfreeze(p))
  }

  private def valueToTerm(name: Value[NamedBc, NamedBc]): NamedBc = name match {
    case LocalVar(name) => mkNamedBc(name)
    case AbsolutePathName(p) =>      mkNamedBc(bc.PathName(p.path))
    case UnitValue =>      mkNamedBc(bc.U)
    case B(b) =>           mkNamedBc(bc.B(b))
    case I(i) =>           mkNamedBc(bc.I(i))
    case S(value) =>       mkNamedBc(bc.S(value))
    case BuiltInChannelName(c) => mkNamedBc(bc.BuiltInChannelName(c))
    case Freeze(p) =>      mkNamedBc(bc.Freeze(p))
  }
}