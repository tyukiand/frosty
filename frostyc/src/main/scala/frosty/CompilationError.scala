package frosty

case class CompilationError(position: Position, message: String) {
  override def toString = "[error] " + position + ": " + message
}