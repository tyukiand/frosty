package frosty.namespace

import frosty.Position

/** Global view of a package structure "as it actually is".
  * Tree of uniquely named entities, where the entities are either 
  * packages or symbols.
  */
sealed trait Global {
  def resolve(absPath: AbsolutePath): Option[Global]
  def position: Position
}
case class GlobSymbol(path: AbsolutePath, position: Position) extends Global {
  def resolve(absPath: AbsolutePath) = None
}
case class GlobPackage(
  entities: Map[String, (VisibilityModifier, Global)],
  position: Position
) extends Global {
  def resolve(absPath: AbsolutePath): Option[Global] = absPath.path match {
    case Nil => Some(this)
    case h :: t => entities.get(h).flatMap(_._2.resolve(AbsolutePath(t)))
  }
}


object GlobPackage {
  def single(modifier: VisibilityModifier, name: String, elem: Global)
  : GlobPackage = GlobPackage(Map(name -> (modifier, elem)), elem.position)
  def empty(pos: Position): GlobPackage = GlobPackage(Map.empty, pos)
}