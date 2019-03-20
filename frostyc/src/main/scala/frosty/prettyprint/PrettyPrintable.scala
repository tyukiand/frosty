package frosty.prettyprint

import scala.collection.breakOut

trait PrettyPrintable {
  def singleLineApparentWidth: Int
  def headerApparentWidth: Int

  /** Starting in column `col` of code at indent level `indent`, tries to
    * print the text in such a way that the apparent width does not exceed
    * `width`.
    *
    * @return layed out text, zero-based column index where the last line ends
    */
  def prettyPrint(col: Int, indent: Int, width: Int): (String, Int)

  /** Returns a `PrettyPrintable` with same raw text content, but with all
    * invisible markup elements removed.
    */
  def dropMarkup: PrettyPrintable

  def asSingleLine: String

  def prettyPrint(width: Int): String = prettyPrint(0, 0, width)._1
}

object PrettyPrintable {
  val Tab = 2
  
  case class Atom(s: String) extends PrettyPrintable {
    lazy val singleLineApparentWidth = s.size
    def headerApparentWidth = singleLineApparentWidth
    def prettyPrint(col: Int, _i: Int, _w: Int) = 
      (s, singleLineApparentWidth + col)
    def asSingleLine = s
    def dropMarkup = this
  }

  case class DelimitedList(
    start: PrettyPrintable,
    content: List[PrettyPrintable],
    end: PrettyPrintable
  ) extends PrettyPrintable {
    lazy val startWidth = start.singleLineApparentWidth
    lazy val endWidth = end.singleLineApparentWidth
    def singleLineApparentWidth = 
      startWidth + endWidth + content.map(_.singleLineApparentWidth).sum

    def asSingleLine = {
      content
      .map(_.asSingleLine)
      .mkString(start.asSingleLine, "", end.asSingleLine)
    }

    def prettyPrint(col: Int, indent: Int, width: Int): (String, Int) = {
      val remaining = width - col
      if (remaining >= singleLineApparentWidth) {
        (asSingleLine, col + singleLineApparentWidth)
      } else {
        val lastLineApparentWidth = indent + endWidth
        val resultString = content.map(x => {
          " " * (indent + Tab) +
          x.prettyPrint(indent + Tab, indent + Tab, width)._1
        }).mkString(
          start.asSingleLine + "\n",
          "\n",
          "\n" + " " * indent + end.asSingleLine
        )

        (resultString, lastLineApparentWidth)
      }
    }

    def headerApparentWidth = startWidth
    def dropMarkup = DelimitedList(
      start.dropMarkup,
      content.map(_.dropMarkup),
      end.dropMarkup
    )
  }

  case class Juxtaposition(text: List[PrettyPrintable])
  extends PrettyPrintable {

    require(text.nonEmpty)

    def asSingleLine = text.map(_.asSingleLine).mkString
    def singleLineApparentWidth = text.map(_.singleLineApparentWidth).sum
    def prettyPrint(col: Int, indent: Int, width: Int): (String, Int) = {
      val bldr = new StringBuilder
      var currCol = col
      for (s <- text) {
        val w = s.singleLineApparentWidth
        if (currCol + w > width) {
          if (currCol + s.headerApparentWidth > width) {
            bldr += '\n'
            bldr ++= " " * indent
            val (pp, c) = s.prettyPrint(indent, indent, width)
            bldr ++= pp
            currCol = c
          } else {
            val (pp, c) = s.prettyPrint(currCol, indent, width)
            bldr ++= pp
            currCol = c
          }
        } else {
          // TODO: merge two branches?
          val (pp, c) = s.prettyPrint(currCol, indent, width)
          bldr ++= pp
          currCol = c
        }
      }
      (bldr.result, currCol)
    }
    def headerApparentWidth = text.head.headerApparentWidth
    def dropMarkup = Juxtaposition(text.map(_.dropMarkup))
  }

  case class Markup(prefix: String, content: PrettyPrintable, suffix: String)
  extends PrettyPrintable {
    def asSingleLine = prefix + content.asSingleLine + suffix
    def singleLineApparentWidth = content.singleLineApparentWidth
    def headerApparentWidth = content.headerApparentWidth
    def prettyPrint(col: Int, indent: Int, width: Int): (String, Int) = {
      val (ppCont, i) = content.prettyPrint(col, indent, width)
      (prefix + ppCont + suffix, i)
    }
    def dropMarkup = content
  }

  /** A more-or-less sane way to print bunch of nested case classes.
    * 
    * Allows to omit pure wrapper classes that only generate noise by 
    * adding them into the `unwrapped` set. 
    * (e.g. various fixpoint wrappers and single-element case classes are 
    * good candidates to be added to the `unwrapped` set). 
    * Annotating a class with `prettyprint.unwrap` has the same effect.
    */
  def fromAny(
    a: Any,
    unwrapped: Set[Class[_ <: Product]] = Set.empty,
    customHandlers: Map[Class[_], Any => PrettyPrintable] = Map.empty
  ): PrettyPrintable = {
    a match {
      case x if x.getClass.isAnnotationPresent(classOf[useToString]) =>
        Atom(x.toString)
      case x if customHandlers.contains(x.getClass) =>
        customHandlers(x.getClass)(x)
      case s: Seq[Any] => {
        val componentPPs: List[PrettyPrintable] =
          s.map(fromAny(_, unwrapped, customHandlers))(breakOut)

        val componentHeads = componentPPs.dropRight(1).map { c =>
          Juxtaposition(List(c, Atom(",")))
        }

        val allComponentsWithSeps = 
          if (componentPPs.isEmpty) componentHeads 
          else (componentHeads :+ componentPPs.last)

        DelimitedList(
          Atom("["),
          allComponentsWithSeps,
          Atom("]")
        )
      }
      case p: Product => {
        if (
          (unwrapped contains p.getClass) ||
          p.getClass.isAnnotationPresent(classOf[unwrap])
        ) {
          try {
            val u = p.productIterator.next
            fromAny(u, unwrapped, customHandlers)
          } catch {
            case e: NoSuchElementException =>
              throw new AssertionError(
                "Assumed that case class annotated with `unwrap` has at " +
                "least one component, but iterator of " + p.getClass +
                " was empty."
              )
          }
        } else {
          val prefix = Atom(
            if (p.getClass.getSimpleName.matches("Tuple\\d+")) ""
            else p.productPrefix
          )
          val components = p.productIterator.toList
          val componentPPs = 
            if (components.nonEmpty) {
              val componentsWithCommas = 
                components
                .dropRight(1)
                .map {
                  c => Juxtaposition(
                    List(fromAny(c, unwrapped, customHandlers), Atom(","))
                  )
                }
              val lastComponent = 
                fromAny(components.last, unwrapped, customHandlers)
              componentsWithCommas :+ lastComponent
            } else {
              Nil
            }
          if (componentPPs.nonEmpty) {
            DelimitedList(
              Juxtaposition(List(prefix, Atom("("))),
              componentPPs,
              Atom(")")
            )
          } else {
            prefix
          }
        }
      }
      case sthElse => Atom(sthElse.toString)
    }
  }
}
