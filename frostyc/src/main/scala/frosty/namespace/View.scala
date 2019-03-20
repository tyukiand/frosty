package frosty.namespace

/** A view of everything that can be identified, either by local or global
  * paths.
  */
case class View(global: Global, local: Local) {
  def mapLocal(f: Local => Local): View = View(global, f(local))
  // TODO: cruft?
  def importConst(l: Local) = Some(this.mapLocal(_ ++ l))

  /** Creates modified view resulting from this view by importing all
    * symbols referenced by an absolute path.
    *
    * Makes all selected public symbols visible.
    */
  def importAbs(p: AbsolutePath, sel: ImportSelector): Option[View] = {
    global.resolve(p).flatMap {
      _ match {
        case g @ GlobPackage(_, _) => sel.selectFrom(Local.externalView(g)).map {
          v => this.mapLocal(_ ++ v)
        }
        case GlobSymbol(_, _) => None
      }
    }
  }

  /** Creates modified view resulting from this view by importing all
    * symbols referenced by a relative path.
    *
    * Makes all selected public symbols visible.
    */
  def importRel(p: RelativePath, sel: ImportSelector): Option[View] = {
    local.resolve(p).flatMap {
      _ match {
        case g @ GlobPackage(_, _) => sel.selectFrom(Local.externalView(g)).map {
          v => this.mapLocal(_ ++ v)
        }
        case GlobSymbol(_, _) => None
      }
    }
  }

  /** Creates modified view resulting from this view by entering
    * a subpackage.
    *
    * This means that all currently visible symbols disappear, and 
    * all public and private symbols of the subpackage become visible.
    */
  def enter(pkgName: String): Option[View] = {
    local
    .lookup(pkgName)
    .flatMap {
      case s: GlobSymbol => None
      case p: GlobPackage => Some(View(global, Local.internalView(p)))
    }
  }
}

object View {
  def fromGlobal(pkg: GlobPackage): View = {
    val local = Local.internalView(pkg)
    View(pkg, local)
  }
}

/** An operation that transforms a view in a certain way, e.g. by importing 
  * more symbols.
  */
/* TODO: looks like cruft. Unclear why reification of view modification
   operations as objects was necessary in the first place. Adding same 
   functionality as methods seems sufficient.
sealed trait ViewOp extends (View => Option[View])

case class ImportConst(l: Local) extends ViewOp {
  def apply(a: View) = Some(a.mapLocal(_ ++ l))
}

case class ImportAbs(p: AbsolutePath, sel: ImportSelector) extends ViewOp {
  def apply(a: View) = // moved
}

case class ImportRel() extends ViewOp {
  def apply(a: View) = // moved
}
*/