package frosty.namespace

/** Visibility modifiers for symbols and packages. */
sealed trait VisibilityModifier
case object Public extends VisibilityModifier
case object Private extends VisibilityModifier
