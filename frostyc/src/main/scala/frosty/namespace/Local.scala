package frosty.namespace

/** Local view of the nested namespace, warped and twisted by multiple 
  * renaming imports etc. Essentially just a map from non-qualified names to
  * the actual `Global` entities.
  */
case class Local(entities: Map[String, Global]) {
  def +(name: String, entity: Global): Local =
    Local(entities + (name -> entity))
  def ++(other: Local): Local = Local(this.entities ++ other.entities)
  def resolve(relPath: RelativePath): Option[Global] = relPath.path match {
    case Nil => None
    case h :: Nil => entities.get(h)
    case h :: t => entities.get(h).flatMap {
      case GlobSymbol(_, _) => None
      case pkg @ GlobPackage(_, _) => {
        Local.externalView(pkg).resolve(RelativePath(t))
      }
    }
  }
  def lookup(name: String): Option[Global] = entities.get(name)
}

object Local {
  /** Hides private symbols in `g`.*/
  def externalView(g: GlobPackage): Local = {
    Local(g.entities.collect { case (n, (Public, e)) => (n, e) })
  }
  /** Exposes private symbols in `g`.*/
  def internalView(g: GlobPackage): Local = {
    Local(g.entities.map { case (k, (_, v)) => (k, v) })
  }
}
