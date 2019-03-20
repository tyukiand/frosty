package frosty

import frosty.ast._
import frosty.algebra.Term

/** Contains just the erased type definitions, and a simple type eraser that
  * replaces all types in a typed ast by their erased versions. 
  *
  * This package exists because it provides a separate name space
  * for the erased types, so that they don't collide with the non-erased ones.
  */
  /* TODO: CRUFT
package object erasure {
  type ErasedAstC[X, Y] = (ErasedType, AstC[X, Y])
  type ErasedAst = Term[ErasedAstC, String, ErasedType]
}
*/