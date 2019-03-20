package frosty.ast
import frosty.types.Type
import frosty.Position
import frosty.namespace.AbsolutePath

case class SymbolDeclaration(name: AbsolutePath, typ: Type, pos: Position)
