package frosty

case class Position(source: SourceCode, line: Int, column: Int) {
  override def toString = s"${source.path}:${line}:${column}"
}