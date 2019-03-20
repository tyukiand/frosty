package frosty

import frosty.algebra._

/** Contains definition of the terms that are serialized into bytes and
  * then passed to the VM.
  */
package object bytecode {
  type NamedBc = Term[Bc, String, Unit]
  def mkNamedBc(t: Bc[NamedBc, Binder[Bc, String, Unit]]): NamedBc = 
    Cons[Bc, String, Unit](t)
  def mkNamedBc(name: String): NamedBc = Var[Bc, String, Unit](name, ())

  type Bytecode = DbTerm[Bc, Unit]
  def mkBc(b: Bc[Bytecode, DbBinder[Bc, Unit]]): Bytecode = DbCons[Bc, Unit](b)
  def par(procs: List[Bytecode]): Bytecode = DbCons[Bc, Unit](Parallel(procs))
  def b(x: Boolean): Bytecode = DbCons[Bc, Unit](B(x))
  def s(s: String): Bytecode = DbCons[Bc, Unit](S(s))
}