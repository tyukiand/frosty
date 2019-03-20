package frosty

import collection.mutable.ListBuffer

object PhaseName {
  class Value(val shortcut: String) {
    override def toString = shortcut
  }
  private val _values: ListBuffer[Value] = ListBuffer.empty
  private object Value {
    def apply(shortcut: String): Value = {
      val v = new Value(shortcut)
      _values += v
      v
    }
  }
  val Input = Value("input")
  val Parse = Value("parse")
  val Resolve = Value("resolve")
  val Type = Value("type")
  val Translate = Value("translate")
  val Desugar = Value("desugar")
  val EliminateExpressions = Value("elim-expr")
  val EmitCode = Value("emit-code")

  lazy val values: List[Value] = _values.toList
  lazy val allShortcuts: List[String] = values.map(_.shortcut)
  private lazy val shortcutMap = (for (v <- values) yield (v.shortcut, v)).toMap
  def isValidShortcut(s: String): Boolean = shortcutMap contains s
  def forShortcut(s: String): Value = shortcutMap(s)
}
