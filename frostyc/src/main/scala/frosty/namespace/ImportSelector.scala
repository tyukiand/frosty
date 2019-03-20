package frosty.namespace

/** The last piece in an import clause, selects one or multiple symbols from
  * the package that has been selected by the preceeding path.
  */
sealed trait ImportSelector {
  def selectFrom(view: Local): Option[Local]
}

case object SelectAll extends ImportSelector {
  def selectFrom(view: Local) = Some(view)
}

case class SelectOne(name: String) extends ImportSelector {
  def selectFrom(view: Local) = view.entities.get(name).map {
    case x => view + (name, x)
  }
}

case class SelectSubselectors(subselectors: List[Subselector])
extends ImportSelector {
  def selectFrom(view: Local) = {
    // TODO: when moved from script into project, replace by State traversal
    @annotation.tailrec
    def rec(
      sels: List[Subselector],
      rem: Map[String, Global],
      acc: List[Map[String, Global]]
    ): Option[Map[String, Global]] = {
      sels match {
        case Nil => Some(acc.reduce(_ ++ _))
        case h :: t => {
          h.subselect(rem) match {
            case Some((addThis, newRem)) => rec(t, newRem, addThis :: acc)
            case None => None
          }
        }
      }
    }

    rec(subselectors, view.entities, Nil).map(Local.apply)
  }
}

/** Component of a multi-subselector import selector, for very specific
  * selections like e.g.:
  * {{{
  *   import `/foo/bar/{a,b,c => d, e => _, _}`
  * }}}
  */
sealed trait Subselector {
  /** Takes the rest of the unselected entities, produces a map
    * that should be added to the view, and the remaining 
    * unselected entities.
    */
  def subselect(unselected: Map[String, Global])
  : Option[(Map[String, Global], Map[String, Global])]
}

case class SubselectOne(name: String) extends Subselector {
  def subselect(unselected: Map[String, Global]) = {
    unselected.get(name).map { g =>
      (Map(name -> g), unselected.filterKeys(_ != name))
    }
  }
}

case class IgnoreOne(name: String) extends Subselector {
  def subselect(unselected: Map[String, Global]) = {
    unselected.get(name).map { g =>
      (Map.empty, unselected.filterKeys(_ != name))
    }
  }
}

case object SubselectRest extends Subselector {
  def subselect(unselected: Map[String, Global]) = {
    Some(unselected, Map.empty)
  }
}

case class SubselectRenameOne(original: String, changed: String)
extends Subselector {
  def subselect(unselected: Map[String, Global]) = {
    unselected.get(original).map { g =>
      (Map(changed -> g), unselected.filterKeys(_ != original))
    }
  }
}
