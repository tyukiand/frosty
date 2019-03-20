package frosty

package object util {
  def indent(s: String) = 
    s.split("\n").map("  " + _).mkString("\n")

  /** Makes `x.pipeForward(f)` syntax available. 
    *
    * Usually used as replacement for `x match { fCases }`,
    * which for whatever reason does not allow to append 
    * further operations after the closing brace.
    */
  implicit class PipeForward[X](x: X) {
    def pipeForward[Y](f: X => Y): Y = f(x)
  }
}

