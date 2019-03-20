package frosty.namespace

import frosty.Position
import frosty.prettyprint

/** Higher-level AST built from nested namespaces, imports, and declarations,
  * that contains simpler term-valued ASTs at the leafs.
  *
  * Emitted by the parser.
  */
sealed trait NamespaceAst[+Ast, +Decl] {
  def position: Position
}

/** Declaration of a function, channel, global constant etc. */
case class Declaration[Decl](
  modifier: VisibilityModifier,
  name: String,
  decl: Decl,
  position: Position
) extends NamespaceAst[Nothing, Decl]

@prettyprint.unwrap
case class Code[Ast](code: Ast, position: Position) 
  extends NamespaceAst[Ast, Nothing]

case class Pkg[A, D](
  modifier: VisibilityModifier,
  name: String,
  content: List[NamespaceAst[A, D]],
  position: Position
) extends NamespaceAst[A, D]

sealed trait ImportAst {
  def position: Position
}
case class ImportRelAst(
  rel: RelativePath,
  sel: ImportSelector,
  position: Position
) extends ImportAst

case class ImportAbsAst(
  abs: AbsolutePath,
  sel: ImportSelector,
  position: Position
) extends ImportAst

case class UnderImport[A, D](i: ImportAst, content: NamespaceAst[A, D])
  extends NamespaceAst[A, D] {

  def position: Position = i.position
}

object NamespaceAst {
}