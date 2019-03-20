package frosty.namespace
import frosty.prettyprint

// TODO: this file name is confusing

/** Path of a symbol (or subpackage) relative to the current package. */

@prettyprint.useToString
case class RelativePath(path: List[String]) {
  override def toString = path.mkString("`", "/", "`")
  def /(name: String): AbsolutePath = AbsolutePath(path :+ name)
}

object RelativePath {
  def apply(s: String*): RelativePath = RelativePath(s.toList)
}

/** Absolute path, starting from the implicit root package.
  */
@prettyprint.useToString
case class AbsolutePath(path: List[String]) {
  override def toString = path.mkString("`/", "/", "`")
  def /(name: String): AbsolutePath = AbsolutePath(path :+ name)
}

object AbsolutePath {
  def apply(s: String*): AbsolutePath = AbsolutePath(s.toList)
}

